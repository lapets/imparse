----------------------------------------------------------------
--
-- Imparse
-- Cross-platform/language parser generator.
--
-- Imparse.hs
--   Haskell implementatio of the Imparse parser parser.
--
--
-- * Usage:
--
--   imparse "path/file.p"
--

----------------------------------------------------------------
-- Main module for the Haskell implementation of the
-- Imparse parser.

module Text.Imparse
  where

import System.Environment (getArgs)
import System.IO

import Text.RichReports (report)
import Text.Ascetic.HTML (html)

import Text.Imparse.AbstractSyntax
import Text.Imparse.Parse (parseParser)
import Text.Imparse.Analysis (Analysis, analyze)

----------------------------------------------------------------
-- The target of the output, as specified by the command-line
-- arguments.

data OutputTarget =
    HTML
  | ASCII
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

procWrite :: [OutputTarget] -> Maybe String -> IO ()
procWrite outs fname =
  do { fname     <- maybe (return "") return fname
     ; txt       <- if length fname > 0 then readFile fname else return ""
     ; parser    <- parse txt
     ; case parser of
         Nothing -> return ()
         Just parser ->
           do { parser <- return $ analyze parser
              ; if HTML `elem` outs then
                  do { txt <- return $ show parser
                     ; writeFile (fname++".html") $ show $ html (report parser)
                     ; putStr $ "  Wrote file \"" ++ fname ++ ".html\".\n"
                     }
                else
                  do return ()
              ; if ASCII `elem` outs then
                  do { txt <- return $ show parser
                     ; writeFile (fname++".txt") txt
                     ; putStr $ "  Wrote file \"" ++ fname ++ ".txt\".\n"
                     }
                else
                do return ()
              }
     }

usage :: IO ()
usage = putStr "\n  Usage:\timparse \"path/file.p\"\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd [] []            = usage
cmd ts ("-html":ss)  = cmd (HTML:ts) ss
cmd ts ("-ascii":ss) = cmd (ASCII:ts) ss
cmd ts [f]           = procWrite ts (Just f)
cmd _ _              = usage

----------------------------------------------------------------
-- The main function.

main :: IO ()
main =
  do{ args <- getArgs
    ; cmd [] args
    }

--eof
