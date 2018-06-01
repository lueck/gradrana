import System.Environment
import System.Exit
import qualified Data.Map as Map
import Data.Default.Class
import Control.Monad.Reader
import Options.Applicative
import Data.Semigroup

import GraDrAna.App
import GraDrAna.Config
import GraDrAna.Opts
import GraDrAna.IO
import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.CoPresence
import GraDrAna.Graph.Common


run :: ConfiguredAppOpts -> IO ()
run opts = runGraDrAnaApp configuredApp (passOpts def opts)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info
           (configuredAppOpts_ <**> helper)
           (fullDesc
            <> progDesc "Choose a method to generate graph data and an output format."
            <> header "tei2graph - generate a graph from a play encoded in TEI format")
