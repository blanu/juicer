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
    (remotename:localname:requestname:[]) -> do
        maybeRemote <- metathaw remotename
        maybeLocal  <- metathaw localname
        case (maybeRemote, maybeLocal) of
            (Nothing, _) -> return ()
            (_, Nothing) -> return ()
            (Just remote, Just local) -> grate remote local requestname
    otherwise         -> putStrLn "grate [remote] [local] [request]"

grate :: Metafeed -> Metafeed -> String -> IO()
grate remote local requestname = do
    putStrLn $ show remote
    putStrLn $ show local
    let request = metadiff remote local
    freezerequest request requestname
    putStrLn $ show request
