module Hecate.Parser
  ( runCLIParser
  ) where

import           Options.Applicative

import           Hecate.Evaluator


descArgP :: Parser String
descArgP = argument str $ metavar "DESC" <> help "Description of ciphertext"

pathArgP :: Parser String
pathArgP = argument str $ metavar "PATH" <> help "Path of CSV file"

hashOptP :: Parser String
hashOptP = strOption $ long "hash"
                    <> short 'h'
                    <> metavar "HASH"
                    <> help "SHA1 Hash of desired entry"

descOptP :: Parser String
descOptP = strOption $ long "description"
                    <> short 'd'
                    <> metavar "DESC"
                    <> help "Description of desired entry"

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

modifyCiphertextFlagP :: Parser ModifyAction
modifyCiphertextFlagP = flag Keep Change $ long "ciphertext"
                                        <> short 'c'
                                        <> help "Modify ciphertext"

verbosityFlagP :: Parser Verbosity
verbosityFlagP = flag Normal Verbose $ long "verbose"
                                    <> short 'v'
                                    <> help "Display verbose results"

addP :: Parser Command
addP = Add <$> descArgP
           <*> optional idenOptP
           <*> optional metaOptP

lookupP :: Parser Command
lookupP = Lookup <$> descArgP
                 <*> optional idenOptP
                 <*> verbosityFlagP

importP :: Parser Command
importP = Import <$> pathArgP

exportP :: Parser Command
exportP = Export <$> pathArgP

exportJSONP :: Parser Command
exportJSONP = ExportJSON <$> pathArgP

modifyP :: Parser Command
modifyP = Modify <$> targetP
                 <*> modifyCiphertextFlagP
                 <*> optional idenOptP
                 <*> optional metaOptP

redescribeP :: Parser Command
redescribeP = Redescribe <$> targetP <*> descArgP

removeP :: Parser Command
removeP = Remove <$> targetP

checkP :: Parser Command
checkP = pure CheckForMultipleKeys

cmdAdd        :: Mod CommandFields Command
cmdLookup     :: Mod CommandFields Command
cmdImport     :: Mod CommandFields Command
cmdExport     :: Mod CommandFields Command
cmdExportJSON :: Mod CommandFields Command
cmdModify     :: Mod CommandFields Command
cmdRedescribe :: Mod CommandFields Command
cmdRemove     :: Mod CommandFields Command
cmdCheck      :: Mod CommandFields Command
cmdAdd        = command "add"         $ info addP        (progDesc "Encrypt a piece of text and add it to the store")
cmdLookup     = command "lookup"      $ info lookupP     (progDesc "Lookup a piece of ciphertext in the store")
cmdImport     = command "import"      $ info importP     (progDesc "Import a CSV file")
cmdExport     = command "export"      $ info exportP     (progDesc "Export a CSV file")
cmdExportJSON = command "export-json" $ info exportJSONP (progDesc "Export a JSON file")
cmdModify     = command "modify"      $ info modifyP     (progDesc "Modify a piece of ciphertext in the store")
cmdRedescribe = command "redescribe"  $ info redescribeP (progDesc "Modify the description of a piece of ciphertext in the store")
cmdRemove     = command "remove"      $ info removeP     (progDesc "Remove a piece of ciphertext from the store")
cmdCheck      = command "check"       $ info checkP      (progDesc "Check if all entries have the same keyid")

master :: Parser Command
master = hsubparser (cmdAdd <> cmdLookup <> cmdImport <> cmdExport <> cmdExportJSON <> cmdModify <> cmdRedescribe <> cmdRemove <> cmdCheck)

opts :: ParserInfo Command
opts = info (master <**> helper) (fullDesc <> progDesc "A minimal password manager")

runCLIParser :: IO Command
runCLIParser = execParser opts
