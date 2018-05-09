import System.Environment
import System.Exit
import qualified Data.Map as Map

import GraDrAna.Tei
import GraDrAna.TypeDefs
import GraDrAna.Identify

main :: IO ()
main = do
  args <- getArgs
  (roles, scenes) <- runTeiParsers (args !! 0)

  -- print roles
  putStrLn $ formatPersons roles

  -- identify
  newReg <- identifySpeakersIO roles scenes

  -- print roles again
  putStrLn $ formatPersons newReg

  -- putStrLn $ show roles
  -- putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  -- putStrLn $ show scenes
  return ()
