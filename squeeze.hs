import System.Environment (getArgs)
import Data.List
import Data.List.Split

import Juicer.Freeze
import Juicer.Puree
import Juicer.Blend

main :: IO ()
main = do
  args <- getArgs
  case args of
    (requestname:archivename:diffname:[]) -> do
        maybeRequest <- requestthaw requestname
        maybeArchive  <- thaw archivename
        case (maybeRequest, maybeArchive) of
            (Nothing, _) -> return ()
            (_, Nothing) -> return ()
            (Just request, Just archive) -> squeeze request archive diffname
    otherwise         -> putStrLn "blend [request] [archive] [diff]"

squeeze :: Request -> Feed -> String -> IO()
squeeze request archive diffname = do
    putStrLn $ show request
    putStrLn $ show archive
    let d = diff archive request
    freezediff d diffname
    putStrLn $ show d
