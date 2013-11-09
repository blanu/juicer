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
    (diffname:archivename:[]) -> do
        maybeDiff <- diffthaw diffname
        maybeArchive  <- thaw archivename
        case (maybeDiff, maybeArchive) of
            (Nothing, _) -> return ()
            (_, Nothing) -> return ()
            (Just diff, Just archive) -> blend diff archive archivename
    otherwise         -> putStrLn "squeeze [diff] [archive]"

blend :: Diff -> Feed -> String -> IO()
blend diff archive archivename = do
    putStrLn $ show diff
    putStrLn $ show archive
    let archive' = merge archive diff
    freeze archive' archivename
    putStrLn $ show archive'
