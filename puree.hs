import Data.List
import Data.List.Split

import Juicer.Freeze
import Juicer.Puree

main :: IO ()
main = do
  maybeFeed <- thaw
  case maybeFeed of
    Nothing -> return ()
    Just (Feed _ posts) -> do
      let post@(Post title desc) = posts !! 0
      printTags desc
      let p = parse desc
      putStrLn $ show p
