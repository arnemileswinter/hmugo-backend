module Domain.Profile where

import ClassyPrelude
import Domain.Blog (BlogContent, Image, NewBlogContent)

type AuthorName = Text
type AuthorInsta = Text
type AuthorBio = BlogContent
type AuthorAvatar = Image
data AuthorProfile = AuthorProfile
    { authorName :: AuthorName
    , authorInsta :: AuthorInsta
    , authorBio :: AuthorBio
    , authorAvatar :: AuthorAvatar
    }

data UpdateAuthorProfile = UpdateAuthorProfile
    { updateAuthorName :: Maybe AuthorName
    , updateAuthorInsta :: Maybe AuthorInsta
    , updateAuthorBio :: Maybe NewBlogContent
    , updateAuthorAvatar :: Maybe ByteString
    }
