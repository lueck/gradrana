{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GraDrAna.App where

import Control.Monad
import Control.Monad.Reader

import GraDrAna.Config


type AppConfig = MonadReader Config

newtype App a = App {
  runA :: ReaderT Config IO a
  } deriving (Monad, Functor, Applicative, MonadIO, MonadReader Config)


runGraDrAnaApp :: App a -> Config -> IO a
runGraDrAnaApp app cfg = runReaderT (runA app) cfg
