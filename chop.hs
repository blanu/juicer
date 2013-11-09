import System.Environment (getArgs)
import Data.List
import Data.List.Split

import Juicer.Freeze
import Juicer.Blend

splitBegin :: String -> String -> (String, String)
splitBegin begin input =
  let parts = splitOn begin input
  in (head parts, intercalate begin $ tail parts)

splitEnd :: String -> String -> (String, String)
splitEnd end input =
  let parts = splitOn end input
  in (intercalate end $ init parts, last parts)

trim :: String -> String -> String -> String
trim begin end input =
  let (_,body,_) = splitBody begin end input
  in body

trimBegin :: String -> String -> String
trimBegin begin input =
  let (head,body) = splitBegin begin input
  in body

trimEnd :: String -> String -> String
trimEnd end input =
  let (body,tail) = splitEnd end input
  in body

trimTag :: String -> String -> String
trimTag tag input = trim ("<"++tag++">") ("</"++tag++">") input

splitBody :: String -> String -> String -> (String, String, String)
splitBody begin end input = do
  let (head,rest) = splitBegin begin input
  let (body,tail) = splitEnd end rest
  (head,body,tail)

splitTag :: String -> String -> [String]
splitTag tag input = do
  let openTag = "<"++tag++">"
  let closeTag = "</"++tag++">"
  let items = splitOn openTag $ trimBegin openTag input
  map (trimEnd closeTag) items

printItems :: [String] -> IO()
printItems [] = return ()
printItems (item:items) = do
  printItem item
  printItems items

printItem :: String -> IO()
printItem item = do
  let title = trimTag "title" item
  let desc = trimTag "description" item
  putStrLn $ "Post Title: " ++ title
  putStrLn $ "Post Description: " ++ desc

getPosts :: [String] -> [(String, String)]
getPosts = map getPost

getPost :: String -> (String, String)
getPost item = do
  let title = trimTag "title" item
  let desc = trimTag "description" item
  (title, desc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (rssname:frozenname:manifestname:[]) -> chopit rssname frozenname manifestname
    otherwise               -> putStrLn "chop [rss] [frozen] [manifest]"

chopit :: String -> String -> String -> IO()
chopit rss frozen manifest = do
  f <- readFile rss
  let channel = trimTag "channel" f
  let (header,_) = splitBegin "<item>" channel
  let title = splitTag "title" header !! 0
  let desc = trimTag "description" header
  putStrLn $ "Feed Title: " ++ title
  putStrLn $ "Feed Description: " ++ desc
  putStrLn "------------------"
  let items = splitTag "item" channel
  putStrLn $ show $ length items
  printItems items
  let posts = getPosts items
  let feed = package (title, desc) posts
  freeze feed frozen
  let mf = metafeed feed
  metafreeze mf manifest
