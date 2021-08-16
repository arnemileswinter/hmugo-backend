module Domain.Blog where

import ClassyPrelude

newtype Image = Image FilePath
newtype CreateImage = CreateImage ByteString

data BlogContentSection
    = TextSection Text
    | ImageSection Image
type BlogContent = [BlogContentSection]

data NewBlogContentSection
    = NewTextSection Text
    | NewImageSection ByteString
type NewBlogContent = [NewBlogContentSection]

data BlogInfo = BlogInfo {blogHeader :: Text, blogMotto :: Text, blogFooterText :: Text, blogBaseURL :: Text}
data UpdateBlogInfo = UpdateBlogInfo {updateBlogHeader :: Maybe Text, updateBlogMotto :: Maybe Text, updateFooterText :: Maybe Text, updateBaseURL :: Maybe Text}
