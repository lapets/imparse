
import Text.RichReports (report, Report(Finalize))
import Text.Ascetic.HTML (html)
import System.Environment (getArgs)

import AbstractSyntax
import Report
import Parse

main :: IO ()
main =
  do{ [f] <- getArgs
    ; txt <- readFile f
    ; r <- return $ parseString txt
    ; case r of
        Left err -> putStr $ show err
        Right t ->
          writeFile "out.html" $ show $ html $ Finalize $ report t
    }

--eof
