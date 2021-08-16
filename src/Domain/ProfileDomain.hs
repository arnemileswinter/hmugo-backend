{-# LANGUAGE LambdaCase #-}

module Domain.ProfileDomain where

import ClassyPrelude
import Domain.Blog
import Domain.Domain
import Domain.Profile

getAuthorProfile :: Domain AuthorProfile
getAuthorProfile = GetAuthorProfile

newBioContent :: NewBlogContent -> Domain BlogContent
newBioContent cnt = forM cnt $
    \case
        NewTextSection txt -> return $ TextSection txt
        NewImageSection bs -> do
            img <- AddBioImage bs
            return $ ImageSection img

updateAuthorProfile :: UpdateAuthorProfile -> Domain AuthorProfile
updateAuthorProfile u0 = do
    p <- getAuthorProfile
    updated <- go p u0
    SaveAuthorProfile updated
    pure updated
  where
    go :: AuthorProfile -> UpdateAuthorProfile -> Domain AuthorProfile
    go p u@UpdateAuthorProfile{updateAuthorInsta = Just l} =
        go p{authorInsta = l} u{updateAuthorInsta = Nothing}
    go p u@UpdateAuthorProfile{updateAuthorName = Just n} =
        go p{authorName = n} u{updateAuthorName = Nothing}
    go p u@UpdateAuthorProfile{updateAuthorAvatar = Just na} = do
        a <- UpdateAuthorAvatar na
        go p{authorAvatar = a} u{updateAuthorAvatar = Nothing}
    go p u@UpdateAuthorProfile{updateAuthorBio = Just nb} = do
        b <- newBioContent nb
        go p{authorBio = b} u{updateAuthorBio = Nothing}
    go p (UpdateAuthorProfile Nothing Nothing Nothing Nothing) = pure p
