----------------------------------------------------------------
--
-- Imparse
-- Cross-platform/-language parser generator.
--
-- Text/Imparse.hs
--   Haskell implementation of the Imparse parser parser.
--

----------------------------------------------------------------
-- Useful functions for the imparser parser generator.

module Text.Imparse
  where

import Data.List (splitAt, elemIndex)
import System.Environment (getArgs)
import System.IO

import qualified Control.Compilation.Compile as C
import qualified Text.UXADT as U (uxadt, javaScriptModule)
import Text.RichReports (report)
import Text.Ascetic.HTML (html)

import Text.Imparse.AbstractSyntax
import Text.Imparse.Report
import Text.Imparse.Parse (parseParser)
import Text.Imparse.Analysis (Analysis, analyze)
import Text.Imparse.ToHaskell

----------------------------------------------------------------
-- The target of the output, as specified by the command-line
-- arguments.

data OutputTarget =
    HTML
  | ASCII
  | UXADT
  | HS
  deriving Eq

----------------------------------------------------------------
-- Take a file path in the form of a string, and try to parse
-- the contents of the file into abstract syntax.

parseShow :: String -> IO ()
parseShow fname =
  do { s <- readFile fname
     ; r <- return $ parseParser s
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
-- Take a file path in the form of a string, read it, and
-- process it as specified by the command line.

nothing :: IO ()
nothing = return ()

fileNamePrefix :: String -> String
fileNamePrefix s = fst $ splitAt (maybe (length s) id (elemIndex '.' s)) s

writeAndPutStr :: String -> String -> String -> IO ()
writeAndPutStr file ext s =
  do { writeFile (file++"."++ext) s
     ; putStr $ "\n  Wrote file \"" ++ file ++ "." ++ ext ++ "\".\n"
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
              ; fname <- return $ fileNamePrefix fname

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
              
              ; if HS `elem` outs then
                  writeAndPutStr fname "hs" (C.extract (toAbstractSyntax parser) "")
                else
                  do nothing
              
              }
     }

usage :: IO ()
usage = putStr "\n  Usage:\timparse [-html] [-ascii] [-uxadt] [-hs] \"path/file.g\"\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd [] []            = usage
cmd ts ("-html":ss)  = cmd (HTML:ts) ss
cmd ts ("-ascii":ss) = cmd (ASCII:ts) ss
cmd ts ("-uxadt":ss) = cmd (UXADT:ts) ss
cmd ts ("-hs":ss)    = cmd (HS:ts) ss
cmd ts [f]           = procWrite ts (Just f)
cmd _ _              = usage

--eof
