{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Domain where

import ClassyPrelude
import qualified Domain.Blog as B
import qualified Domain.Post as P
import qualified Domain.Profile as A

data Domain t where
    Pure :: a -> Domain a
    Bind :: Domain a -> (a -> Domain b) -> Domain b
    Publish :: Domain ()
    Revert :: Domain ()
    GetChanges :: Domain [Text]
    GetCurrentTime :: Domain UTCTime
    {-blog info-}
    SaveBlogInfo :: B.BlogInfo -> Domain ()
    GetBlogInfo :: Domain B.BlogInfo
    {-author-}
    GetAuthorProfile :: Domain A.AuthorProfile
    SaveAuthorProfile :: A.AuthorProfile -> Domain ()
    UpdateAuthorAvatar :: ByteString -> Domain B.Image
    AddBioImage :: ByteString -> Domain B.Image
    {-posts-}
    FindPosts :: P.PostPagination -> Domain [P.PostID]
    GetPostPages :: P.PostPagination -> Domain Int
    GetPost :: P.PostID -> Domain P.Post
    NextPostID :: P.PostTitle -> Domain P.PostID
    AddPostImage :: P.PostID -> ByteString -> Domain B.Image
    SavePost :: P.Post -> Domain ()
    DeletePost :: P.PostID -> Domain ()

-- required instances to make Domain a free monad.
instance Functor Domain where
    aToB `fmap` a = a >>= Pure . aToB
instance Applicative Domain where
    pure = Pure
    aToB <*> a = aToB >>= flip fmap a
instance Monad Domain where
    (>>=) = Bind

data Error
    = ErrorUnknown Text
    | ErrorNotFound Text

instance Show Error where
    show (ErrorUnknown t) = "unknown error: " <> unpack t
    show (ErrorNotFound t) = "not found: " <> unpack t
