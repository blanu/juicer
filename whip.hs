import System.Environment (getArgs)
import Data.List
import Data.List.Split

import Juicer.Freeze
import Juicer.Puree
import Juicer.Blend
import Juicer.Whip

main :: IO ()
main = do
  args <- getArgs
  case args of
    (archivename:jsonname:[]) -> do
        maybeArchive  <- thaw archivename
        case maybeArchive of
            Nothing      -> return ()
            Just archive -> whip archive jsonname
    otherwise         -> putStrLn "whip [archive] [json]"

whip :: Feed -> String -> IO()
whip archive jsonname = do
    putStrLn $ show archive
    let json = jsonify archive
    freezejson json jsonname
    putStrLn json
