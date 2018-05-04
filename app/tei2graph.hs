import System.Environment
import System.Exit
import Text.XML.HXT.Core
import qualified Data.Map as Map

import GraDrAna.Tei

main :: IO ()
main = do
  args <- getArgs
  tree <- runX (readDocument [withValidate no] (args !! 0) >>>
                propagateNamespaces)
  roles <- runX (constL tree //>
                 single parseRegisterOfPersons)
  putStrLn $ show roles
  putStrLn $ "Found " ++ (show $ Map.size $ head roles) ++ " persons in the registry."
  scenes <- runX (constL tree //>
                  multi parseScene)
  putStrLn $ "Found " ++ show (length scenes) ++ " scenes."
  putStrLn $ show scenes
