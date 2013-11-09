{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Juicer.Blend
(
 Metafeed(..),
 metadiff,
 diff,
 merge,
 metafeed
)
where

import System.IO
import Data.Serialize
import GHC.Generics
import Data.List
import qualified Data.ByteString as B

import Juicer.Freeze

metafeed :: Feed -> Metafeed
metafeed (Feed channel posts) = Metafeed channel (map extractID posts)

extractID :: Metapost -> PostID
extractID (Metapost hashID _) = hashID

metadiff :: Metafeed -> Metafeed -> Request -- remote local diff
metadiff (Metafeed channelR postsR) (Metafeed channelL postsL) = Request $ postsdiff postsR postsL

postsdiff :: [PostID] -> [PostID] -> [PostID]
postsdiff postsR postsL = (nub postsR) \\ (nub postsL)

diff :: Feed -> Request -> Diff
diff (Feed channel posts) (Request ids) = Diff channel $ extractPosts posts ids

extractPosts :: [Metapost] -> [PostID] -> [Metapost]
extractPosts posts ids = filter (idmatch ids) posts

idmatch :: [PostID] -> Metapost -> Bool
idmatch ids metapost@(Metapost hashID _) = elem hashID ids

merge :: Feed -> Diff -> Feed
merge (Feed _ posts) (Diff channel posts') = Feed channel $ mergePosts posts posts'

mergePosts :: [Metapost] -> [Metapost] -> [Metapost]
mergePosts a b = union a b
