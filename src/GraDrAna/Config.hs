-- | A default configuration for a GraDrAna application. The 'Config'
-- type is defined in 'GraDrAna.App'.

module GraDrAna.Config where

import Data.Default.Class

import GraDrAna.App

import GraDrAna.IO
import GraDrAna.Tei
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.CoPresence

-- | Default configuration for an application.
instance Default Config where
  def = Config
    { _cfg_inFile = Nothing
    , _cfg_outFile = Nothing
    , _cfg_logFile = Nothing
    , _cfg_logLevel = 100
    , _cfg_loadContents = loadContents
    , _cfg_parsePlay = runTeiParsers
    , _cfg_identifyAddSpeakers = identifySpeakersAdd
    , _cfg_adjustRoleIds = adjustRoleIds
    , _cfg_splitScenes = splitByScene
    , _cfg_genGraphData = copresence
    , _cfg_exportGraph = copresenceGraphmlWriter
    }
