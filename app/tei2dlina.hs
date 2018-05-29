import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.TurnQuantity

main :: IO ()
main = do
  args <- getArgs

  (roles, turnQuant) <- runTeiParsers (args !! 0) >>=
    uncurry identifySpeakersAddIO >>=
    uncurry adjustRoleIdsIO >>=
    uncurry splitBySceneIO >>=
    uncurry dlinaIO

  putStrLn $ show turnQuant

  return ()
