module Interpreter.SerializeBlogContent (serializeBlogContent) where

import ClassyPrelude
import Domain.Blog

type ToPublicPath = FilePath -> Text

serializeBlogSection :: ToPublicPath -> BlogContentSection -> ByteString
serializeBlogSection _ (TextSection txt) = "\n" <> encodeUtf8 txt <> "<!-- #endsection -->\n\n"
serializeBlogSection toPublicPath (ImageSection (Image fp)) =
    "\n![](" <> encodeUtf8 (toPublicPath fp) <> ")"
        <> "<!-- #static "
        <> encodeUtf8 (pack fp)
        <> " -->"
        <> "<!-- #endsection -->\n"

serializeBlogContent :: ToPublicPath -> BlogContent -> ByteString
serializeBlogContent toPublicPath = concatMap (serializeBlogSection toPublicPath)
