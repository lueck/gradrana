{-# LANGUAGE FlexibleContexts #-}

-- | Parsers for command line options of GraDrAna applications.

module GraDrAna.Opts where

import Options.Applicative
import Data.Semigroup
import Control.Lens

import GraDrAna.App
import GraDrAna.IO
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.CoPresence

data Splitter = Scene | TimeSlice

data Graph = CoPresence | TurnQuantity

data GraphOutputFormat = GraphML | RawPersons | RawTurns

data ConfiguredAppOpts = ConfiguredAppOpts
  { _cao_playIn :: Maybe FilePath
  , _cao_graphOut :: Maybe FilePath
  , _cao_splitter :: Splitter
  , _cao_graph :: Graph
  , _cao_graphOutputFormat :: GraphOutputFormat
  }

playIn_ :: Parser (Maybe FilePath)
playIn_ = optional $ strOption
  ( long "in"
  <> short 'i'
  <> metavar "INFILE"
  <> help "Path to the file containing the play. If this option is not given, the play is read from standard input.")

graphOut_ :: Parser (Maybe FilePath)
graphOut_ = optional $ strOption
  ( long "out"
  <> short 'o'
  <> metavar "OUTFILE"
  <> help "Path to the file, where the graph data are be written to. If this option is not given, the output is written to standard output.")

splitter_ :: Parser Splitter
splitter_ = scene_ <|> timeSlice_

scene_ :: Parser Splitter
scene_ = flag TimeSlice Scene
  ( long "scene"
  <> help "Split the play by scenes. This is considered to be too simplistic.")

timeSlice_ :: Parser Splitter
timeSlice_ = flag' TimeSlice
  ( long "timeslice"
  <> help "Split the play by time slices. This is considered to be more accurate than splitting by scenes. It is the default splitting method.")

graph_ :: Parser Graph
graph_ = copresence_ <|> turnQuantity_

copresence_ :: Parser Graph
copresence_ = flag' CoPresence
  ( long "copresence"
  <> short 'C'
  <> help "Generate the graph data based on co-presence of persons in a time slice (scene). This generates an undirected graph, where the edges' labels are integers and represent the count of time slices, two persons share.")

turnQuantity_ :: Parser Graph
turnQuantity_ = flag CoPresence TurnQuantity
  ( long "turnquantity"
  <> short 'Q'
  <> help "Generate the graph data based on the quantity of the persons turns i.e. the amount of utterances. The quantity is calculated from the count of turns, the amount of words uttered, the amount of characters uttered, the amount of lines filled by a person, when the other person is present. This generates a directed graph.")

graphOutputFormat_ :: Parser GraphOutputFormat
graphOutputFormat_ = graphml_ <|> rawPersons_ <|> rawTurns_

graphml_ :: Parser GraphOutputFormat
graphml_ = flag' GraphML
  ( long "graphml"
  <> help "Output the graph in GraphML format. This is the default.")

rawPersons_ :: Parser GraphOutputFormat
rawPersons_ = flag GraphML RawPersons
  ( long "raw-persons"
  <> help "Output the registry of persons as raw haskell data. This makes sense for debugging.")

rawTurns_ :: Parser GraphOutputFormat
rawTurns_ = flag GraphML RawTurns
  ( long "raw-turns"
  <> help "Output the turns as raw haskell data. This makes sense for debugging.")

configuredAppOpts_ :: Parser ConfiguredAppOpts
configuredAppOpts_ = ConfiguredAppOpts
  <$> playIn_
  <*> graphOut_
  <*> splitter_
  <*> graph_
  <*> graphOutputFormat_


passOpts :: Config -> ConfiguredAppOpts -> Config
passOpts cfg opts
  | otherwise -- _cao_graph opts == CoPresence
  = cfgCommon opts
    (cfg
     & cfg_genGraphData .~ copresence
     & cfg_exportGraph .~ (case (_cao_graphOutputFormat opts) of
                             GraphML -> copresenceGraphmlWriter
                             RawPersons -> rawPersonsOut
                             RawTurns -> rawTurnsOut))
  where
    cfgCommon opts cfg =
      cfg
      & cfg_inFile .~ (_cao_playIn opts)
      & cfg_outFile .~ (_cao_graphOut opts)
      & cfg_splitScenes .~ (getSplitter $ _cao_splitter opts)
    getSplitter Scene = splitByScene
    getSplitter TimeSlice = splitByScene -- FIXME
  

rawTurnsOut :: Show a => b -> a -> App [Int]
rawTurnsOut _ a = do
  writeOutput $ show a
  return [0::Int]

rawPersonsOut :: Show a => a -> b -> App [Int]
rawPersonsOut a _ = do
  writeOutput $ show a
  return [0::Int]
