{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module Juicer.Freeze
(
 Feed(..),
 Channel(..),
 Post(..),
 PostID(..),
 Metafeed(..),
 Metapost(..),
 Request(..),
 Diff(..),
 package,
 freeze,
 thaw,
 metathaw,
 freezerequest,
 requestthaw,
 metafreeze,
 freezediff,
 diffthaw,
 freezejson
)
where

import System.IO
import Data.Serialize
import GHC.Generics
import qualified Data.ByteString as B
import Crypto.Hash.Skein512

import Juicer.Puree

data Request = Request [PostID] deriving (Generic, Show)
instance Serialize Request
data Diff = Diff Channel [Metapost] deriving (Generic, Show)
instance Serialize Diff

data Feed = Feed Channel [Metapost] deriving (Generic, Show)
instance Serialize Feed

data Channel = Channel String String deriving (Generic, Show) -- title description
instance Serialize Channel

data Metafeed = Metafeed Channel [PostID] deriving (Generic, Show)
instance Serialize Metafeed
data Metapost = Metapost PostID Post deriving (Generic, Show) -- hash post
instance Serialize Metapost
instance Eq Metapost where
    (==) (Metapost ida _) (Metapost idb _) = ida == idb

data PostID = PostID B.ByteString deriving (Generic, Show, Eq)
instance Serialize PostID
data Post = Post String Content deriving (Generic, Show) -- title description
instance Serialize Post

package :: (String, String) -> [(String, String)] -> Feed
package (title, desc) posts = Feed (Channel (fixNbsp title) (fixNbsp desc)) (packagePosts posts)

packagePosts :: [(String, String)] -> [Metapost]
packagePosts = map packageMetapost

packageMetapost :: (String, String) -> Metapost
packageMetapost pair =
    let post = packagePost pair
        hashID = hash 64 $ encode post
    in Metapost (PostID hashID) post

packagePost :: (String, String) -> Post
packagePost (title, desc) = Post (fixNbsp title) (parse desc)

freeze :: Feed -> String -> IO()
freeze feed path = B.writeFile path $ encode feed

metafreeze :: Metafeed -> String -> IO()
metafreeze metafeed path = B.writeFile path $ encode metafeed

freezerequest :: Request -> String -> IO()
freezerequest request requestname = B.writeFile requestname $ encode request

freezediff :: Diff -> String -> IO()
freezediff request requestname = B.writeFile requestname $ encode request

freezejson :: String -> String -> IO()
freezejson archive jsonname = writeFile jsonname archive

thaw :: String -> IO(Maybe Feed)
thaw path = do
    bs <- B.readFile path
    let eitherFeed = decode bs
    case eitherFeed of
        Left error -> do
            putStrLn $ "Error thawing: " ++ show error
            return Nothing
        Right feed -> return $ Just feed

metathaw :: String -> IO(Maybe Metafeed)
metathaw path = do
    bs <- B.readFile path
    let eitherFeed = decode bs
    case eitherFeed of
        Left error -> do
            putStrLn $ "Error thawing: " ++ show error
            return Nothing
        Right feed -> return $ Just feed

requestthaw :: String -> IO(Maybe Request)
requestthaw path = do
    bs <- B.readFile path
    let eitherFeed = decode bs
    case eitherFeed of
        Left error -> do
            putStrLn $ "Error thawing: " ++ show error
            return Nothing
        Right feed -> return $ Just feed

diffthaw :: String -> IO(Maybe Diff)
diffthaw path = do
    bs <- B.readFile path
    let eitherFeed = decode bs
    case eitherFeed of
        Left error -> do
            putStrLn $ "Error thawing: " ++ show error
            return Nothing
        Right feed -> return $ Just feed
