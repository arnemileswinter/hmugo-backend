{-# LANGUAGE LambdaCase #-}

module Application.JsonTypes where

import ClassyPrelude
import Control.Monad (fail)
import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import Domain.Blog
import Domain.Post
    ( CreatePost(CreatePost),
      Post(Post),
      PostID,
      PostInformation(PostInformation, postInfoLastMod,
                      postInfoCreatedAt, postInfoCoverImage, postInfoTags,
                      postInfoCategories, postInfoTitle),
      PostTitle,
      UpdatePost(UpdatePost) )
import Domain.Profile

jsonImg :: MonadIO m => Image -> m Value
jsonImg (Image fp) = String . decodeUtf8 . B64.encode <$> readFile fp

jsonPaginatedPosts :: Int -> [(PostID, PostTitle)] -> Value
jsonPaginatedPosts numPages is =
    object
        [ "posts" .= Array (fromList $ map (\(i, t) -> object ["id" .= i, "title" .= t]) is)
        , "pages" .= numPages
        ]

jsonAuthorProfile :: MonadIO m => AuthorProfile -> m Value
jsonAuthorProfile (AuthorProfile n l b i) = do
    img <- jsonImg i
    blg <- jsonBlogContent b
    pure $
        object
            [ "name" .= n
            , "insta" .= l
            , "bio" .= blg
            , "image" .= img
            ]

jsonBlogContent :: (Monad m, MonadIO m) => BlogContent -> m Value
jsonBlogContent bc = do
    a <- forM bc $ \case
        TextSection txt -> pure $ object ["type" .= ("txt" :: Text), "val" .= txt]
        ImageSection im -> jsonImg im >>= \i -> pure $ object ["type" .= ("img" :: Text), "val" .= i]
    pure $ Array $ fromList a

jsonBlogInfo :: Monad m => BlogInfo -> m Value
jsonBlogInfo (BlogInfo header motto footerText baseURL) =
    pure $
        object
            [ "header" .= header
            , "motto" .= motto
            , "footerText" .= footerText
            , "baseURL" .= baseURL
            ]

jsonPost :: (Monad m, MonadIO m) => Post -> m Value
jsonPost
    ( Post
            pid
            PostInformation
                { postInfoLastMod = lm
                , postInfoCreatedAt = ca
                , postInfoCoverImage = ci
                , postInfoTags = ts
                , postInfoCategories = cs
                , postInfoTitle = t
                }
            cnt
        ) = do
        cnt' <- jsonBlogContent cnt
        ci' <- jsonImg ci
        pure $
            object
                [ "id" .= pid
                , "title" .= t
                , "updatedAt" .= lm
                , "createdAt" .= ca
                , "tags" .= ts
                , "categories" .= cs
                , "cover" .= ci'
                , "content" .= cnt'
                ]

newtype JsonUpdateBlogInfo = JsonUpdateBlogInfo UpdateBlogInfo
instance FromJSON JsonUpdateBlogInfo where
    parseJSON (Object v) =
        JsonUpdateBlogInfo
            <$> ( UpdateBlogInfo
                    <$> v .:? "header"
                    <*> v .:? "motto"
                    <*> v .:? "footerText"
                    <*> v .:? "baseURL"
                )
    parseJSON _ = fail "not a JSON object!"

newtype JsonNewBlogContent = JsonNewBlogContent NewBlogContent
instance FromJSON JsonNewBlogContent where
    parseJSON (Array a) =
        JsonNewBlogContent . fmap (\(JsonNewBlogContentSection sec) -> sec)
            <$> sequenceA (parseJSON <$> toList a)
    parseJSON _ = fail "not a JSON array!"

newtype JsonNewBlogContentSection = JsonNewBlogContentSection NewBlogContentSection
instance FromJSON JsonNewBlogContentSection where
    parseJSON (Object v) = do
        t <- v .: "type"
        JsonNewBlogContentSection
            <$> ( case (t :: Text) of
                    "img" -> do
                        NewImageSection . (\(JsonCreateImage (CreateImage im)) -> im) <$> (v .: "val")
                    "txt" -> NewTextSection <$> (v .: "val")
                    e -> fail $ "type field has unknown value: " <> show e
                )
    parseJSON _ = fail "not a JSON object!"

newtype JsonUpdateAuthorProfile = JsonUpdateAuthorProfile UpdateAuthorProfile
instance FromJSON JsonUpdateAuthorProfile where
    parseJSON (Object v) = do
        bio <- v .:? "bio"
        image <- v .:? "image"
        JsonUpdateAuthorProfile
            <$> ( UpdateAuthorProfile
                    <$> v .:? "name"
                    <*> v .:? "insta"
                    <*> pure (fmap (\(JsonNewBlogContentSection sec) -> sec) <$> (bio :: Maybe [JsonNewBlogContentSection]))
                    <*> pure ((\(JsonCreateImage (CreateImage bs)) -> bs) <$> (image :: Maybe JsonCreateImage))
                )
    parseJSON _ = fail "not a JSON object!"

newtype JsonCreateImage = JsonCreateImage CreateImage
instance FromJSON JsonCreateImage where
    parseJSON (String v) =
        either
            (\e -> fail $ "could not decode image: " <> e)
            (pure . JsonCreateImage . CreateImage)
            (B64.decode $ encodeUtf8 v)
    parseJSON _ = fail "image is not text!"

newtype JsonUpdatePost = JsonUpdatePost UpdatePost
instance FromJSON JsonUpdatePost where
    parseJSON (Object v) = do
        im <- fmap (\(JsonCreateImage i) -> i) <$> v .:? "image"
        cnt <- fmap (\(JsonNewBlogContent i) -> i) <$> v .:? "content"
        JsonUpdatePost
            <$> ( pure (UpdatePost im)
                    <*> v .:? "tags"
                    <*> v .:? "categories"
                    <*> v .:? "title"
                    <*> pure cnt
                )
    parseJSON _ = fail "not a JSON object!"

newtype JsonCreatePost = JsonCreatePost CreatePost
instance FromJSON JsonCreatePost where
    parseJSON (Object v) = do
        (JsonCreateImage im) <- v .: "image"
        (JsonNewBlogContent bc) <- v .: "content"
        JsonCreatePost
            <$> ( pure (CreatePost im)
                    <*> v .: "tags"
                    <*> v .: "categories"
                    <*> v .: "title"
                    <*> pure bc
                )
    parseJSON _ = fail "not a JSON object!"
