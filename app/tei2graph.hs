import System.Environment
import System.Exit
import qualified Data.Map as Map
import Data.Default.Class
import Control.Monad.Reader

import GraDrAna.App
import GraDrAna.Config
import GraDrAna.IO
import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.CoPresence
import GraDrAna.Graph.Common

main :: IO ()
main = runGraDrAnaApp configuredApp def

app :: App ()
app = do
  (roles, scenes) <-
    runTeiParsers >>=
    uncurry identifySpeakersAdd >>=
    uncurry adjustRoleIds >>=
    uncurry splitByScene >>=
    uncurry copresence
  
  uncurry copresenceGraphmlWriter (roles, scenes)

  liftIO $ putStrLn $ formatPersons roles
  liftIO $ putStrLn $ show $ undirected $ rmLoops roles

  liftIO $ putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  --putStrLn $ show scenes

  return ()
