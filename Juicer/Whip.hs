{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Juicer.Whip
(
 jsonify
)
where

import System.IO
import Data.List
import Data.String.Utils
import qualified Data.ByteString as B

import Juicer.Freeze
import Juicer.Puree

jsonify :: Feed -> String
jsonify (Feed channel posts) = "{" ++ (jsonifyChannel channel) ++ ", " ++ (jsonifyPosts posts) ++ "}"

jsonifyChannel :: Channel -> String
jsonifyChannel (Channel title desc) = "\"channel\": {\"title\":  \"" ++ (quote title) ++ "\",\"description\": \"" ++ (quote desc) ++ "\"}"

jsonifyPosts :: [Metapost] -> String
jsonifyPosts posts = "\"items\": [" ++ (intercalate "," (map jsonifyPost posts)) ++ "]"

jsonifyPost :: Metapost -> String
jsonifyPost (Metapost _ (Post title desc)) = "{\"title\":  \"" ++ (quote title) ++ "\",\"description\": \"" ++ (quote $ htmlize desc) ++ "\"}"

htmlize :: Content -> String
htmlize (Content elements) = concat (map htmlizeElement elements)

htmlizeElement :: Element -> String
htmlizeElement (Image url maybeHeight maybeWidth) =
    case (maybeHeight, maybeWidth) of
        (Nothing, Nothing)        -> "<img src=\"" ++ url ++ "\"/>"
        (Just height, Nothing)    -> "<img src=\"" ++ url ++ "\" height=\"" ++ (show height) ++ "\"/>"
        (Nothing, Just width)     -> "<img src=\"" ++ url ++ "\" width=\"" ++ (show width) ++ "\"/>"
        (Just height, Just width) -> "<img src=\"" ++ url ++ "\" height=\"" ++ (show height) ++ "\" width=\"" ++ (show width) ++ "\"/>"
htmlizeElement (Link title maybeUrl) =
    case maybeUrl of
        Nothing  -> "<a href=\"\">" ++ title ++ "</a>"
        Just url -> "<a href=\"" ++ url ++ "\">" ++ title ++ "</a>"
htmlizeElement (Text s) = s

quote :: String -> String
quote = replace "\"" "\\\""
