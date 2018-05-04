import System.Environment
import System.Exit
import Text.XML.HXT.Core

import GraDrAna.Tei

main :: IO ()
main = do
  args <- getArgs
  ps <- runX (readDocument [withValidate no] (args !! 0) >>>
              propagateNamespaces //>
              single parseRegisterOfPersons)
  putStrLn $ show ps
  putStrLn $ show $ length ps
