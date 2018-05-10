import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify

main :: IO ()
main = do
  args <- getArgs

  (roles, scenes) <- runTeiParsers (args !! 0) >>=
    uncurry identifySpeakersAddIO >>=
    uncurry adjustRoleIdsIO

  putStrLn $ formatPersons roles
  putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  putStrLn $ show scenes

  return ()
