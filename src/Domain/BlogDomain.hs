module Domain.BlogDomain where

import ClassyPrelude
import Domain.Blog
import Domain.Domain

publish :: Domain ()
publish = Publish

getChanges :: Domain [Text]
getChanges = GetChanges

revert :: Domain ()
revert = Revert

getBlogInfo :: Domain BlogInfo
getBlogInfo = GetBlogInfo

updateBlogInfo :: UpdateBlogInfo -> Domain BlogInfo
updateBlogInfo u0 = do
    inf <- GetBlogInfo
    updated <- go inf u0
    SaveBlogInfo updated
    pure updated
  where
    go :: BlogInfo -> UpdateBlogInfo -> Domain BlogInfo
    go b u@UpdateBlogInfo{updateBlogHeader = Just h} = go b{blogHeader = h} u{updateBlogHeader = Nothing}
    go b u@UpdateBlogInfo{updateBlogMotto = Just h} = go b{blogMotto = h} u{updateBlogMotto = Nothing}
    go b u@UpdateBlogInfo{updateFooterText = Just h} = go b{blogFooterText = h} u{updateFooterText = Nothing}
    go b u@UpdateBlogInfo{updateBaseURL = Just h} = go b{blogBaseURL = h} u{updateBaseURL = Nothing}
    go b (UpdateBlogInfo Nothing Nothing Nothing Nothing) = pure b
