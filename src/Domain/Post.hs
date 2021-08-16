module Domain.Post where

import ClassyPrelude
import Domain.Blog (BlogContent, CreateImage, Image, NewBlogContent)

type PostID = Text
type Time = UTCTime
type Tag = Text
type Category = Text
type PostTitle = Text
data PostInformation = PostInformation
    { postInfoID :: PostID
    , postInfoLastMod :: Time
    , postInfoCreatedAt :: Time
    , postInfoCoverImage :: Image
    , postInfoTags :: [Tag]
    , postInfoCategories :: [Category]
    , postInfoTitle :: PostTitle
    }
data Post = Post
    { postId :: PostID
    , postInfo :: PostInformation
    , postContent :: BlogContent
    }

data UpdatePost = UpdatePost
    { updatePostCoverImage :: Maybe CreateImage
    , updatePostTags :: Maybe [Tag]
    , updatePostCategories :: Maybe [Category]
    , updatePostTitle :: Maybe PostTitle
    , updatePostContent :: Maybe NewBlogContent
    }

data CreatePost = CreatePost
    { createPostCoverImage :: CreateImage
    , createPostTags :: [Tag]
    , createPostCategories :: [Category]
    , createPostTitle :: PostTitle
    , createPostContent :: NewBlogContent
    }

data PostPagination = PostPagination {postPaginationOffset :: Int, postPaginationLimit :: Int}
