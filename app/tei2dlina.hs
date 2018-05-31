import System.Environment
import System.Exit
import qualified Data.Map as Map
import Data.Default.Class
import Control.Monad.Reader

import GraDrAna.App
import GraDrAna.Config
import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.TurnQuantity


main :: IO ()
main = runGraDrAnaApp app def

app :: App ()
app = do
  args <- liftIO getArgs

  (roles, turnQuant) <- runTeiParsers (args !! 0) >>=
    uncurry identifySpeakersAddIO >>=
    uncurry adjustRoleIdsIO >>=
    uncurry splitByScene >>=
    uncurry dlina

  liftIO $ putStrLn $ show turnQuant

  return ()
