----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse.hs@
--
--   Haskell implementation of the Imparse parser parser.
--   Useful functions for the imparser parser generator.
--

----------------------------------------------------------------
--

module Text.Imparse
  where

import Data.Char (toUpper)
import Data.List (splitAt, elemIndex)
import System.Directory (
    createDirectory, 
    removeDirectoryRecursive, 
    doesDirectoryExist, 
    doesFileExist, 
    removeFile
  )
import System.Environment (getArgs)
import System.IO ()
import Prelude hiding (catch)
import System.IO.Error hiding (catch)
import Control.Exception (throwIO, catch)

import Control.Compilation.String (compiled)
import qualified Text.UxADT as U (uxadt, javaScriptModule)
import Text.RichReports (report)
import Text.Ascetic.HTML (html)

import Text.Imparse.AbstractSyntax
import Text.Imparse.Report
import Text.Imparse.Parse (parseParser)
import Text.Imparse.Analysis (Analysis, analyze)
import Text.Imparse.Compile.Haskell

----------------------------------------------------------------
-- | The target of the output, as specified by the command-line
--   arguments.

type HaskellModulePrefix = String

data OutputTarget =
    HTML
  | ASCII
  | UXADT
  | HASKELL HaskellModulePrefix
  deriving Eq

emitHaskell :: [OutputTarget] -> Maybe HaskellModulePrefix
emitHaskell ots = case ots of
  []           -> Nothing
  HASKELL p :_ -> Just p
  ot:ots       -> emitHaskell ots

----------------------------------------------------------------
-- | Take a file path in the form of a string, and try to parse
--   the contents of the file into abstract syntax.

parseShow :: String -> IO ()
parseShow fname =
  do { s <- readFile fname
     ; r <- return $ (parseParser s :: Either String (Parser Analysis))
     ; case r of
         Left err -> do { putStr "parse error: "; putStr err }
         Right parser ->
           do { putStr $ show parser
              }
     }

parse :: String -> IO (Maybe (Parser Analysis))
parse str =
  do { r <- return $ parseParser str
     ; case r of
         Left err -> do { putStr "parse error: "; putStr err ; return Nothing }
         Right top -> return $ Just top
     }

----------------------------------------------------------------
-- | Take a file path in the form of a string, read it, and
--   process it as specified by the command line.

nothing :: IO ()
nothing = return ()

createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists dir = 
  do chk <- doesDirectoryExist dir
     if chk then nothing else createDirectory dir

removeIfExists :: FilePath -> IO ()
removeIfExists file = removeFile file `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

fileNamePrefix :: String -> String
fileNamePrefix s = fst $ splitAt (maybe (length s) id (elemIndex '.' s)) s

fileNameDir :: String -> String
fileNameDir s = 
  if '/' `elem` s then
    (fst $ splitAt (maybe (length s) id (elemIndex '/' s)) s) ++ "/"
  else
    ""

writeAndPutStr :: String -> String -> String -> IO ()
writeAndPutStr file ext s =
  do { writeFile (file++"."++ext) s
     ; putStr $ "  Wrote file \"" ++ file ++ "." ++ ext ++ "\".\n"
     }

procWrite :: [OutputTarget] -> Maybe String -> IO ()
procWrite outs fname =
  do { fname     <- maybe (return "") return fname
     ; txt       <- if length fname > 0 then readFile fname else return ""
     ; parser    <- parse txt
     ; case parser of
         Nothing -> return ()
         Just parser ->
           do { parser <- return $ analyze parser
              ; (fname, fdir) <- return $ (fileNamePrefix fname, fileNameDir fname)
              ; putStr "\n"

              ; if HTML `elem` outs then
                  writeAndPutStr fname "html" (show $ html $ report parser)
                else
                  do nothing

              ; if ASCII `elem` outs then
                  writeAndPutStr fname "txt" (show parser)
                else
                  do nothing

              ; if UXADT `elem` outs then
                  writeAndPutStr fname "js" (U.javaScriptModule fname (U.uxadt parser))
                else
                  do nothing
              
              ; case emitHaskell outs of
                  Nothing  -> do nothing
                  Just pre ->
                    do moduleName <- return $ (\(c:cs) -> toUpper c : cs) fname
                       putStr $ "  Emitting Haskell implementation of \"" ++ moduleName ++ "\"...\n"
                       writeAndPutStr (fdir ++ "AbstractSyntax") "hs" (compiled (toAbstractSyntax pre parser))
                       writeAndPutStr (fdir ++ "Report") "hs" (compiled (toRichReport pre parser))
                       writeAndPutStr (fdir ++ "Parse") "hs" (compiled (toParsec pre parser))
                       putStr "\n"
              }
     }

usage :: IO ()
usage = putStr $
     "\nUsage:\n\n"
  ++ "  imparse [optional flags] path/file.p\n\n"
  ++ "Flags:\n\n"
  ++ "   -html\n"
  ++ "   Emit HTML report containing parser static analysis results.\n\n"
  ++ "   -hs \"Name.Prefix.For.Modules\"\n"
  ++ "   Emit Haskell implementations of abstract syntax, parser, and\n   report generator with specified module name prefix.\n\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd [] []            = usage
cmd ts ("-html":ss)  = cmd (HTML:ts) ss
cmd ts ("-ascii":ss) = cmd (ASCII:ts) ss
cmd ts ("-uxadt":ss) = cmd (UXADT:ts) ss
cmd ts ("-hs":p:ss)  = cmd (HASKELL p:ts) ss
cmd ts [f]           = procWrite ts (Just f)
cmd _ _              = usage

--eof
