{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Juicer.Puree
(
 Content(..),
 Element(..),
 parse,
 printTags,
 fixNbsp
)
where

import GHC.Generics
import Data.Serialize
import Text.HTML.TagSoup
import Web.Encodings
import Data.String.Utils
import qualified Data.Map as M

data Element =
    Image String (Maybe Integer) (Maybe Integer) -- url height width
  | Text String
  | Link String (Maybe String) -- title url
  deriving (Generic, Show)
instance Serialize Element

data Content = Content [Element] deriving (Generic, Show)
instance Serialize Content

parse :: String -> Content
parse s = do
    let tags = parseTags $ decodeHtml s
    Content $ decodeTags tags

fixNbsp :: String -> String
fixNbsp = replace "\160" " "

printTags :: String -> IO()
printTags s = putStrLn $ show $ parseTags $ decodeHtml s

decodeTags :: [Tag String] -> [Element]
decodeTags [] = []
decodeTags ((TagOpen "a" _):(TagOpen "img" _):(TagClose "img"):(TagClose "a"):tags) = decodeTags tags
decodeTags ((TagOpen "img" params):(TagClose "img"):tags) = do
    let fields = M.fromList params
    let maybeSrc = M.lookup "src" fields
    case maybeSrc of
        Nothing  -> decodeTags tags
        Just src -> do
            let maybeHeight = M.lookup "height" fields
            let maybeWidth  = M.lookup "width" fields
            case (maybeHeight, maybeWidth) of
                (Just "1" , Nothing)      -> (decodeTags tags)
                (Nothing, Just "1")       -> (decodeTags tags)
                (Just "1", Just "1")      -> (decodeTags tags)
                (Nothing, Nothing)        -> (Image src Nothing Nothing):(decodeTags tags)
                (Just height, Nothing)    -> (Image src (Just (read height :: Integer)) Nothing):(decodeTags tags)
                (Nothing, Just width)     -> (Image src Nothing (Just (read width :: Integer))):(decodeTags tags)
                (Just height, Just width) -> (Image src (Just (read height :: Integer)) (Just(read width :: Integer))):(decodeTags tags)
decodeTags ((TagOpen "a" params):(TagText title):(TagClose "a"):tags) = do
    let fields = M.fromList params
    let maybeHref = M.lookup "href" fields
    case maybeHref of
        Nothing   -> decodeTags tags
        Just href -> (Link (fixNbsp title) $ Just href):(decodeTags tags)
decodeTags ((TagText s):tags) = do
    case s of
        "\160"    -> decodeTags tags
        otherwise -> (Text $ fixNbsp s):(decodeTags tags)
decodeTags (_:tags) = decodeTags tags
