module Application.RestUtil where

import ClassyPrelude
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Word8 as W8
import Network.HTTP.Types (hAuthorization, status200, status401)
import Network.Wai (Middleware, Request (requestHeaders, requestMethod), mapResponseHeaders, modifyResponse, responseBuilder)

withCors :: Middleware
withCors app req sendResponse =
    let corsHeaders =
            [ (CI.mk "access-control-allow-origin", encodeUtf8 $ pack "*")
            , (CI.mk "access-control-allow-methods", encodeUtf8 "GET,POST,PUT,DELETE,PATCH,OPTIONS")
            , (CI.mk "access-control-allow-headers", encodeUtf8 "Authorization,Content-Type")
            ]
     in case requestMethod req of
            "OPTIONS" -> sendResponse $ responseBuilder status200 corsHeaders mempty
            _ -> modifyResponse (mapResponseHeaders (corsHeaders ++)) app req sendResponse

withApiKeyHeader :: String -> Middleware
withApiKeyHeader apiKey app req sendResponse =
    let unAuthorized = sendResponse $ responseBuilder status401 [] mempty
     in case lookup hAuthorization (requestHeaders req) >>= extractBearerAuth of
            Just bb ->
                if bb == encodeUtf8 (pack apiKey)
                    then app req sendResponse
                    else unAuthorized
            Nothing -> unAuthorized

extractBearerAuth :: ByteString -> Maybe ByteString
extractBearerAuth bs =
    let (x, y) = break W8.isSpace bs
     in if BS.map W8.toLower x == "bearer"
            then Just $ dropWhile W8.isSpace y
            else Nothing
