module Interpreter.ParseBlogContent (parseBlogContent) where

import ClassyPrelude
import Data.Attoparsec.ByteString (Parser, parseOnly, satisfy, skipWhile, (<?>))
import Data.Attoparsec.ByteString.Char8 (anyChar, char)
import Data.Attoparsec.ByteString.Lazy (manyTill, string)
import Data.Char (ord)
import Data.Word8 (isSpace)
import Domain.Blog (BlogContent, BlogContentSection (ImageSection, TextSection), Image (Image))

-- | parse a single valid URL character.
urlChar :: Parser Word8
urlChar = satisfy (`elem` encodeUtf8 (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-._~:/?#[]@!$&'()*+,;%=")) <?> "url-character"

-- | skip whitespace except newline.
optionalNonNewlineSpace :: Parser ()
optionalNonNewlineSpace = skipWhile (\c -> isSpace c && c /= (fromIntegral . ord) '\n')

optionalSpace :: Parser ()
optionalSpace = skipWhile isSpace

-- | parses a hugo markdown image tag such as ![](/path/to/test.jpg), yielding only the URL.
pubImgPath :: Parser ByteString
pubImgPath = pack <$> (string "![](" *> manyTill urlChar (char ')'))

-- | parses the static markdown comment that follows the pubImgPath, such as <!-- static /static/to/test.jpg --> yielding only the URL
statImgPath :: Parser ByteString
statImgPath = pack <$> (string "<!-- #static " *> manyTill urlChar (string " -->"))

endSection :: Parser ()
endSection = optionalSpace *> string "<!-- #endsection -->" *> optionalSpace

img :: Parser BlogContentSection
img = do
    optionalSpace
    _ <- pubImgPath
    optionalNonNewlineSpace
    stat <- Image . unpack . decodeUtf8 <$> statImgPath
    endSection
    pure $ ImageSection stat

txt :: Parser BlogContentSection
txt = TextSection . decodeUtf8 . pack <$> manyTill (fromIntegral . ord <$> anyChar) endSection

blogSection :: Parser BlogContentSection
blogSection = img <|> txt

blogContent :: Parser BlogContent
blogContent = char '\n' *> many blogSection

parseBlogContent :: ByteString -> Either String BlogContent
parseBlogContent = parseOnly blogContent
