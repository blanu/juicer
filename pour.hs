import System.Environment (getArgs)
import Data.List
import Data.List.Split
import Control.Monad
import System.Console.ANSI

import Juicer.Freeze
import Juicer.Puree

main :: IO()
main = do
  args <- getArgs
  case args of
    (archivename:[]) -> do
        maybeArchive  <- thaw archivename
        case maybeArchive of
            Nothing -> return ()
            Just archive -> pour archive
    otherwise         -> putStrLn "pour [archive]"

pour :: Feed -> IO ()
pour (Feed (Channel title desc) posts) = do
    channelTitleStyle
    putStrLn title
    channelDescStyle
    putStrLn desc
    textStyle
    putStrLn "---------------------------------------------------"
    mapM_ showPost posts
    textStyle

showPost :: Metapost -> IO()
showPost (Metapost _ (Post title desc)) = do
    titleStyle
    putStrLn title
    textStyle
    showContent desc
    putStrLn ""

showContent :: Content -> IO()
showContent (Content elements) = do
    mapM_ showElement elements
    putStrLn ""

showElement :: Element -> IO()
showElement (Text s) = do
    textStyle
    putStr s
showElement (Link title maybeHref) = do
    linkStyle
    putStr title
showElement (Image link maybeHeight maybeWidth) = do
    imageStyle
    putStr link

channelTitleStyle :: IO()
channelTitleStyle = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Green]

channelDescStyle :: IO()
channelDescStyle = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Magenta]

titleStyle :: IO()
titleStyle = setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]

textStyle :: IO()
textStyle = setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull White]

linkStyle :: IO()
linkStyle = setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Red]

imageStyle :: IO()
imageStyle = setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Blue]
