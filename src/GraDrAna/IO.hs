module GraDrAna.IO where

import System.IO
import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Bool as B

import GraDrAna.App


-- | Load the contents of input file given in '_cfg_inFile'. If this
-- is 'Nothing' read from stdin instead.
loadContents :: App String
loadContents = maybe readStdIn readInFile =<< asks _cfg_inFile
  where
    readStdIn = liftIO $ hGetContents stdin
    readInFile = liftIO . readFile

-- | Write to the output file given in '_cfg_outFile'. If this is
-- 'Nothing' write to stdout instead.
writeOutput :: String -> App ()
writeOutput r = maybe writeStdOut writeOutFile =<< asks _cfg_outFile
  where
    writeStdOut = liftIO $ hPutStrLn stdout r
    writeOutFile = liftIO . (flip writeFile r)


type LogLevel = Int

logLevel :: LogLevel -> String -> App ()
logLevel level msg = do
  minLevel <- asks _cfg_logLevel
  B.bool (return ()) (logAllways msg) (minLevel <= level)

logAllways :: String -> App ()
logAllways msg = maybe logStdIn logFile =<< asks _cfg_logFile
  where
    logStdIn = liftIO $ hPutStrLn stderr msg
    logFile = liftIO . (flip appendFile msg)
