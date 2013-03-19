----------------------------------------------------------------
--
-- Imparse
-- Cross-language parser generator.
--
-- Main.hs
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

module Main
  where

import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec (ParseError)

import Parser.AbstractSyntax ()
import Parser.Parser (parseString)

----------------------------------------------------------------
-- The target of the output, as specified by the command-line
-- arguments.

data OutputTarget =
    None
  deriving Eq

----------------------------------------------------------------
-- Take a file path in the form of a string, and try to parse
-- the contents of the file into abstract syntax.

parseShow :: String -> IO ()
parseShow fname =
  do { s <- readFile fname
     ; r <- return $ parseString s
     ; case r of
         Left err -> do { putStr "parse error: "; print (err :: ParseError) }
         Right prgm ->
           do { putStr $ show prgm
              }
     }

parse :: String -> IO (Maybe Top)
parse str =
  do { r <- return $ parseString str
     ; case r of
         Left err -> do { putStr "parse error: "; print (err :: ParseError) ; return Nothing }
         Right top -> return $ Just top
     }

----------------------------------------------------------------
-- Take a file path in the form of a string, read it, and
-- process it as specified by the command line.

procWrite :: [OutputTarget] -> Maybe String -> IO ()
procWrite outs fname =
  do { fname     <- maybe (return "") return fname
     ; txt       <- if length fname > 0 then readFile fname else return ""
     ; top       <- parse txt
     ; case top of
         Nothing -> return ()
         Just top ->
           do if JS `elem` outs then
                do { js <- return $ extract $ JS.compile top
                   ; putStr $ "\n  Wrote file \"" ++ fname ++ ".js\".\n"
                   ; writeFile (fname++".js") $ js
                   }
              else
                do return ()
     }

usage :: IO ()
usage = putStr "\n  Usage:\timparse \"path/file.p\"\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd []      []            = usage
cmd []      [f]           = usage
cmd outs    [f]           = procWrite outs (Just f)
cmd _ _                   = usage

----------------------------------------------------------------
-- The main function.

main :: IO ()
main =
  do{ args <- getArgs
    ; cmd [] args
    }

--eof
