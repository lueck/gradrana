import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify
import GraDrAna.Splitter.Scene
import GraDrAna.Graph.CoPresence

main :: IO ()
main = do
  args <- getArgs

  (roles, scenes) <- runTeiParsers (args !! 0) >>=
    uncurry identifySpeakersAddIO >>=
    uncurry adjustRoleIdsIO >>=
    uncurry splitBySceneIO >>=
    uncurry copresenceIO

  putStrLn $ formatPersons roles
  putStrLn $ show roles

  putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  putStrLn $ show scenes

  return ()
