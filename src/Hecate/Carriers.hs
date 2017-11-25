{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hecate.Carriers
  ( SQLiteStoreT
  , runSQLiteStoreT
  , AppM
  , runAppM
  ) where

import           Control.Monad.Catch       (MonadThrow (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (MonadReader (..), ReaderT, asks, runReaderT)
import           Control.Monad.Trans.Class (MonadTrans (..))
import qualified Database.SQLite.Simple    as SQLite
import           Lens.Family2

import           Hecate.Data               (AppContext)
import qualified Hecate.Database           as DB
import           Hecate.Interfaces


newtype SQLiteStoreT m a = SQLiteStoreT { runSQLiteStoreT :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           )

mapSQLiteStoreT :: (m a -> n b) -> SQLiteStoreT m a -> SQLiteStoreT n b
mapSQLiteStoreT f = SQLiteStoreT . f . runSQLiteStoreT
{-# INLINE mapSQLiteStoreT #-}

instance MonadTrans SQLiteStoreT where
  lift = SQLiteStoreT
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (SQLiteStoreT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadReader r m => MonadReader r (SQLiteStoreT m) where
  ask    = lift ask
  local  = mapSQLiteStoreT . local
  reader = lift . reader

instance (Monad m, MonadReader r m, HasAppContext r) => MonadConfigReader (SQLiteStoreT m) where
  askConfig = asks (view appContextConfig)

withConnection
  :: (MonadReader r m, HasAppContext r)
  => (SQLite.Connection -> m a)
  -> m a
withConnection f = ask >>= \ ctx -> f (ctx ^. appContextConnection)

instance (MonadThrow m, MonadIO m, MonadReader r m, HasAppContext r) =>
         MonadStore (SQLiteStoreT m) where
  put             e       = withConnection (\ conn -> DB.put             conn e)
  delete          e       = withConnection (\ conn -> DB.delete          conn e)
  query           q       = withConnection (\ conn -> DB.query           conn q)
  selectAll               = withConnection (\ conn -> DB.selectAll       conn)
  getCount                = withConnection (\ conn -> DB.getCount        conn)
  getCountOfKeyId kid     = withConnection (\ conn -> DB.getCountOfKeyId conn kid)
  createTable             = withConnection (\ conn -> DB.createTable     conn)
  migrate         sv  kid = withConnection (\ conn -> DB.migrate         conn sv kid)
  currentSchemaVersion    = pure DB.currentSchemaVersion

instance MonadEncrypt m => MonadEncrypt (SQLiteStoreT m) where
  encrypt kid pt = lift (encrypt kid pt)
  decrypt        = lift . decrypt

instance MonadInteraction m => MonadInteraction (SQLiteStoreT m)  where
  now                              = lift now
  doesFileExist                    = lift . doesFileExist
  doesDirectoryExist               = lift . doesDirectoryExist
  createDirectory                  = lift . createDirectory
  openSQLiteFile                   = lift . openSQLiteFile
  closeSQLiteConnection            = lift . closeSQLiteConnection
  readFileAsString                 = lift . readFileAsString
  readFileAsText                   = lift . readFileAsText
  readFileAsLazyByteString         = lift . readFileAsLazyByteString
  writeFileFromString fp s         = lift (writeFileFromString fp s)
  writeFileFromLazyByteString fp s = lift (writeFileFromLazyByteString fp s)
  getEnv                           = lift . getEnv
  message                          = lift . message
  prompt                           = lift . prompt

instance MonadAppError m => MonadAppError (SQLiteStoreT m) where
  csvDecodingError    = lift . csvDecodingError
  tomlError           = lift . tomlError
  configurationError  = lift . configurationError
  gpgError            = lift . gpgError
  databaseError       = lift . databaseError
  fileSystemError     = lift . fileSystemError
  ambiguousInputError = lift . ambiguousInputError
  migrationError      = lift . migrationError
  defaultError        = lift . defaultError

instance MonadThrow m => MonadThrow (SQLiteStoreT m) where
  throwM = lift . throwM

-- * AppM

newtype AppM a
  = AppM { unAppM :: SQLiteStoreT (ReaderT AppContext IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppContext
           , MonadConfigReader
           , MonadStore
           , MonadEncrypt
           , MonadInteraction
           , MonadAppError
           )

instance MonadThrow AppM where
  throwM = liftIO . throwM

runAppM :: AppM a -> AppContext -> IO a
runAppM m = runReaderT (runSQLiteStoreT (unAppM m))
