{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | We define a monad transformer stack for our application to avoid
-- boilerplate code for passing around the configuration etc.
--
-- See http://book.realworldhaskell.org/read/monad-transformers.html
-- for an introduction on this pattern.

module GraDrAna.App where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import System.Exit
import Text.XML.HXT.DOM.TypeDefs (c_err)

import GraDrAna.TypeDefs

-- | The monad transformer stack for the application.
newtype App a = App {
  runA :: ReaderT Config IO a
  } deriving (Monad, Functor, Applicative, MonadIO, MonadReader Config)

-- | We definitively do not want to allow side effects everywhere. So
-- we can use 'AppConfig' instead of 'App', to make the config
-- available but regain purity and make GHC fail on IO.
--
-- USAGE:
--
-- funWithoutSideEffect :: AppConfig m => Argument -> m ()
type AppConfig = MonadReader Config

-- | Run an application in the 'App' monad transformer stack.
runGraDrAnaApp :: App a -> Config -> IO a
runGraDrAnaApp app cfg = runReaderT (runA app) cfg

-- | An application composed of functions given in the configuration.
--
-- USAGE:
--
-- main :: IO ()
-- main = runGraDrAnaApp configuredApp def
--
-- 'def' from 'Data.Default.Class' in this context returns a default
-- configuration and can be modified using lenses.
configuredApp :: App ()
configuredApp = do
  parsePlay <- asks _cfg_parsePlay
  identifyAddSpeakers <- asks _cfg_identifyAddSpeakers
  adjustRoleIds <- asks _cfg_adjustRoleIds
  split <- asks _cfg_splitScenes
  genGraphData <- asks _cfg_genGraphData
  exportGraph <- asks _cfg_exportGraph
  [rc] <-
    parsePlay >>=
    uncurry identifyAddSpeakers >>=
    uncurry adjustRoleIds >>=
    uncurry split >>=
    uncurry genGraphData >>=
    uncurry exportGraph
  liftIO $ exitWith ( if rc >= c_err
                      then ExitFailure 1
                      else ExitSuccess
                    )

-- | A record for the configuration of a GraDrAna application.
data Config =
  Config
  { _cfg_inFile :: Maybe FilePath -- ^ Nothing means stdin
  , _cfg_outFile :: Maybe FilePath -- ^ Nothing means stdout
  , _cfg_logFile :: Maybe FilePath -- ^ Nothing means stderr
  , _cfg_logLevel :: Int -- ^ Log level
  , _cfg_loadContents :: App String
  , _cfg_parsePlay :: App (Persons, [Scene])
  , _cfg_identifyAddSpeakers :: Persons -> [Scene] -> App (Persons, [Scene])
  , _cfg_adjustRoleIds :: Persons -> [Scene] -> App (Persons, [Scene])
  , _cfg_splitScenes :: Persons -> [Scene] -> App (Persons, [[Turn]])
  , _cfg_genGraphData :: Persons -> [[Turn]] -> App (Persons, [[Turn]])
  , _cfg_exportGraph :: Persons -> [[Turn]] -> App [Int]
  }

makeLenses ''Config
