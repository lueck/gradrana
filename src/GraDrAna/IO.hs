module GraDrAna.IO where

import System.IO
import Control.Monad.Trans
import Control.Monad.Reader

import GraDrAna.App
import GraDrAna.Config

-- | Load the contents of input file given in '_cfg_inFile'. If this
-- is 'Nothing' read from stdin instead.
loadContents :: App String
loadContents = maybe readStdIn readInFile =<< asks _cfg_inFile
  where
    readStdIn = liftIO $ hGetContents stdin
    readInFile = liftIO . readFile
