module Interpreter.HugoUtil where

import ClassyPrelude
import Data.Aeson (Value (Object, String), decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

readHeader :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text)
readHeader = readHugoConfigParam "headerTitle"

writeHeader :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeHeader = changeHugoConfigParam "headerTitle"

readMotto :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text)
readMotto = readHugoConfigParam "motto"

writeMotto :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeMotto = changeHugoConfigParam "motto"

readFooterText :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text)
readFooterText = readHugoConfigParam "footerBottomText"

writeFooterText :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeFooterText = changeHugoConfigParam "footerBottomText"

readBaseURL :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text) 
readBaseURL = readHugoConfig "baseURL"

writeBaseURL :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeBaseURL = changeHugoConfig "baseURL"

readAuthorName :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text) 
readAuthorName = readHugoConfigParam "author"

writeAuthorName :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeAuthorName n = do 
    w <- changeHugoConfigParam "author" n
    either (pure.Left) (\_ -> changeHugoConfig "title" n) w

readAuthorInstagram :: (MonadReader FilePath m, MonadIO m) => m (Either Text Text) 
readAuthorInstagram  = readHugoConfigParam "instagram"

writeAuthorInstagram :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text ())
writeAuthorInstagram = changeHugoConfigParam "instagram"

{-
 | Retrieve a string value from the global hugo site configuration.
-}
readHugoConfig :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text Text)
readHugoConfig s = do
    siteConfigPath <- ask
    configContents <- liftIO $ readFile siteConfigPath
    case decode $ BL.fromStrict configContents of
        Nothing -> do
            pure $ Left "Could not decode site config!"
        Just obj -> do
            case HM.lookup s obj of
                Nothing -> pure $ Left $ "Config has no field called " <> s <> "."
                Just (String m) -> pure $ Right m
                _ -> pure $ Left $ "Configuration field " <> s <> " is not a string."
{-
 | Change a string value within the global hugo site configuration.
-}
changeHugoConfig :: (MonadReader FilePath m, MonadIO m) => Text -> Text -> m (Either Text ())
changeHugoConfig s v0 = do
    siteConfigPath <- ask
    configContents <- readFile siteConfigPath
    case decode $ BL.fromStrict configContents of
        Nothing -> do
            pure $ Left "Could not decode site config!"
        Just obj -> do
            let newConfig = HM.insert s (String v0) obj
            writeFile siteConfigPath $ BL.toStrict $ encode newConfig
            pure $ Right ()
            
{-
 | Retrieve a value from the hugo site configurations "params" object.
-}
readHugoConfigParam :: (MonadReader FilePath m, MonadIO m) => Text -> m (Either Text Text)
readHugoConfigParam s = do
    siteConfigPath <- ask
    configContents <- liftIO $ readFile siteConfigPath
    case decode $ BL.fromStrict configContents of
        Nothing -> do
            pure $ Left "Could not decode site config!"
        Just obj -> do
            case HM.lookup (T.pack "params") obj of
                Nothing -> pure $ Left "Config has no field called params."
                Just (Object v) ->
                    case HM.lookup s v of
                        Just (String m) -> pure $ Right m
                        _ -> pure $ Left $ "Config has no field params." <> s
                _ -> pure $ Left "Config field params is not an object."


{-
 | Change a value within the hugo site configuration "params" object.
-}
changeHugoConfigParam :: (MonadReader FilePath m, MonadIO m) => Text -> Text -> m (Either Text ())
changeHugoConfigParam s v0 = do
    siteConfigPath <- ask
    configContents <- readFile siteConfigPath
    case decode $ BL.fromStrict configContents of
        Nothing -> do
            pure $ Left "Could not decode site config!"
        Just obj -> do
            let eNewParams = case HM.lookup (T.pack "params") obj of
                    Nothing -> Left "Config has no field called params."
                    Just (Object v) ->
                        Right $
                            HM.insert
                                s
                                (String v0)
                                v
                    _ -> Left "Config field params is not an object."
            case eNewParams of
                Left err -> pure $ Left err
                Right newParams -> do
                    let newConfig =
                            HM.insert
                                (T.pack "params")
                                (Object newParams)
                                obj
                    writeFile siteConfigPath $ BL.toStrict $ encode newConfig
                    pure $ Right ()
