module Hecate.Parser
  ( runCLIParser
  ) where

import Data.Monoid ((<>))
import Options.Applicative

import Hecate.Evaluator hiding (eval)


descArgP :: Parser String
descArgP = argument str $ metavar "DESC" <> help "Description of ciphertext"

pathArgP :: Parser String
pathArgP = argument str $ metavar "PATH" <> help "Path of CSV file"

hashOptP :: Parser String
hashOptP = strOption $ long "hash"
                    <> short 'h'
                    <> metavar "HASH"
                    <> help "SHA1 Hash of entry to remove"

descOptP :: Parser String
descOptP = strOption $ long "description"
                    <> short 'd'
                    <> metavar "DESC"
                    <> help "Description of entry to remove"

idenOptP :: Parser String
idenOptP = strOption $ long "identity"
                    <> short 'i'
                    <> metavar "ID"
                    <> help "Identity associated with ciphertext"

metaOptP :: Parser String
metaOptP = strOption $ long "metadata"
                    <> short 'm'
                    <> metavar "META"
                    <> help "Metadata associated with ciphertext"

targetP :: Parser Target
targetP = (TargetId <$> hashOptP) <|> (TargetDescription <$> descOptP)

ciphertextFlagP :: Parser ModifyAction
ciphertextFlagP = flag Keep Change $ long "ciphertext"
                                  <> short 'c'
                                  <> help "Modify ciphertext"

verbosityFlagP :: Parser Verbosity
verbosityFlagP = flag Normal Verbose $ long "verbose"
                                    <> short 'v'
                                    <> help "Display verbose results"

addP :: Parser Command
addP = Add <$> descArgP <*> optional idenOptP <*> optional metaOptP

lookupP :: Parser Command
lookupP = Lookup <$> descArgP <*> verbosityFlagP

importP :: Parser Command
importP = Import <$> pathArgP

modifyP :: Parser Command
modifyP = Modify <$> modificationP <*> ciphertextFlagP <*> optional idenOptP <*> optional metaOptP
  where
    modificationP = (TargetId <$> hashOptP) <|> (TargetDescription <$> descOptP)

redescribeP :: Parser Command
redescribeP = Redescribe <$> targetP <*> descArgP

removeP :: Parser Command
removeP = Remove <$> targetP

cmdAdd        :: Mod CommandFields Command
cmdLookup     :: Mod CommandFields Command
cmdImport     :: Mod CommandFields Command
cmdModify     :: Mod CommandFields Command
cmdRedescribe :: Mod CommandFields Command
cmdRemove     :: Mod CommandFields Command
cmdAdd        = command "add"        $ info addP        (progDesc "Encrypt a piece of text and add it to the store")
cmdLookup     = command "lookup"     $ info lookupP     (progDesc "Lookup a piece of ciphertext in the store")
cmdImport     = command "import"     $ info importP     (progDesc "Import a CSV file")
cmdModify     = command "modify"     $ info modifyP     (progDesc "Modify a piece of ciphertext in the store")
cmdRedescribe = command "redescribe" $ info redescribeP (progDesc "Modify the description of a piece of ciphertext in the store")
cmdRemove     = command "rm"         $ info removeP     (progDesc "Remove a piece of ciphertext from the store")

master :: Parser Command
master = hsubparser (cmdAdd <> cmdLookup <> cmdImport <> cmdModify <> cmdRedescribe <> cmdRemove)

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A minimal password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
