module Paths_imparse (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "D:\\Documents and Settings\\Administrator\\Application Data\\cabal\\bin"
libdir     = "D:\\Documents and Settings\\Administrator\\Application Data\\cabal\\imparse-0.0.0.1\\ghc-7.4.2"
datadir    = "D:\\Documents and Settings\\Administrator\\Application Data\\cabal\\imparse-0.0.0.1"
libexecdir = "D:\\Documents and Settings\\Administrator\\Application Data\\cabal\\imparse-0.0.0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "imparse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "imparse_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "imparse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "imparse_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
