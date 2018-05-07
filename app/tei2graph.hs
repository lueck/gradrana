import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei
import GraDrAna.TypeDefs

main :: IO ()
main = do
  args <- getArgs
  (roles, scenes) <- runTeiParsers (args !! 0)
  putStrLn $ formatPersons roles
  putStrLn $ show roles
  putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  putStrLn $ show scenes
