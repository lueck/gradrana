import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei

main :: IO ()
main = do
  args <- getArgs
  (roles, scenes) <- runTeiParsers (args !! 0)
  putStrLn $ "Found " ++ (show $ Map.size roles) ++ " persons in the registry."
  putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  putStrLn $ show scenes
