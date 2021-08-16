{-# LANGUAGE LambdaCase #-}

module Domain.PostDomain where

import ClassyPrelude
import Domain.Blog
import Domain.Domain
import Domain.Post

getPages :: PostPagination -> Domain Int
getPages = GetPostPages

findPosts :: PostPagination -> Domain [PostID]
findPosts = FindPosts

getPost :: PostID -> Domain Post
getPost = GetPost

updatePost :: PostID -> UpdatePost -> Domain Post
updatePost pid u0 = do
    p <- getPost pid
    updated <- go p u0
    SavePost updated
    pure updated
  where
    go :: Post -> UpdatePost -> Domain Post
    go p u@UpdatePost{updatePostCategories = (Just cats)} =
        go p{postInfo = (postInfo p){postInfoCategories = cats}} u{updatePostCategories = Nothing}
    go p u@UpdatePost{updatePostTags = (Just tags)} =
        go p{postInfo = (postInfo p){postInfoTags = tags}} u{updatePostTags = Nothing}
    go p u@UpdatePost{updatePostCoverImage = (Just cov)} =
        do
            cov' <- newPostImage pid cov
            go p{postInfo = (postInfo p){postInfoCoverImage = cov'}} u{updatePostCoverImage = Nothing}
    go p u@UpdatePost{updatePostTitle = Just t} =
        go p{postInfo = (postInfo p){postInfoTitle = t}} u{updatePostTitle = Nothing}
    go p u@UpdatePost{updatePostContent = Just c} =
        do
            c' <- newPostContent pid c
            go p{postContent = c'} u{updatePostContent = Nothing}
    go p (UpdatePost Nothing Nothing Nothing Nothing Nothing) =
        pure p

newPostImage :: PostID -> CreateImage -> Domain Image
newPostImage pid (CreateImage bs) = AddPostImage pid bs

newPostContent :: PostID -> NewBlogContent -> Domain BlogContent
newPostContent pid cnt = forM cnt $
    \case
        NewTextSection txt -> return $ TextSection txt
        NewImageSection bs -> do
            img <- AddPostImage pid bs
            return $ ImageSection img

createPost :: CreatePost -> Domain Post
createPost
    CreatePost
        { createPostCategories = cats
        , createPostTags = tags
        , createPostTitle = title
        , createPostCoverImage = im
        , createPostContent = cnt
        } = do
        pid <- NextPostID title
        now <- GetCurrentTime
        coverImage <- newPostImage pid im
        pc <- newPostContent pid cnt
        let pinf =
                PostInformation
                    { postInfoID = pid
                    , postInfoCategories = cats
                    , postInfoTitle = title
                    , postInfoCreatedAt = now
                    , postInfoLastMod = now
                    , postInfoTags = tags
                    , postInfoCoverImage = coverImage
                    }
        let newPost = Post pid pinf pc
        SavePost newPost
        pure newPost

deletePost :: PostID -> Domain ()
deletePost = DeletePost
