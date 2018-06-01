{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GraDrAna.App where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import System.Exit
import Text.XML.HXT.DOM.TypeDefs (c_err)

import GraDrAna.TypeDefs

type AppConfig = MonadReader Config

newtype App a = App {
  runA :: ReaderT Config IO a
  } deriving (Monad, Functor, Applicative, MonadIO, MonadReader Config)


runGraDrAnaApp :: App a -> Config -> IO a
runGraDrAnaApp app cfg = runReaderT (runA app) cfg


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
