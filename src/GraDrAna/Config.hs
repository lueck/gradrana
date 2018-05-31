{-# LANGUAGE TemplateHaskell #-}
module GraDrAna.Config where

import Control.Lens
import Data.Default.Class

data Config =
  Config
  { _cfg_inFile :: Maybe FilePath -- ^ Nothing means stdin
  , _cfg_outFile :: Maybe FilePath -- ^ Nothing means stdout
  , _cfg_logFile :: Maybe FilePath -- ^ Nothing means stderr
  }

makeLenses ''Config

instance Default Config where
  def = Config
    { _cfg_inFile = Nothing
    , _cfg_outFile = Nothing
    , _cfg_logFile = Nothing
    }
