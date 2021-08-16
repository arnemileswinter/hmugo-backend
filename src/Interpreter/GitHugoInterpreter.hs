{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter.GitHugoInterpreter where

import ClassyPrelude
import qualified Codec.Picture as JP
import Control.Monad (fail)
import Control.Monad.Except (MonadError (throwError), liftEither, ExceptT (ExceptT))
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson
import Data.Char (isAlphaNum)
import Data.Frontmatter (IResult (Done), parseYamlFrontmatter)
import Data.YAML.Aeson (encode1)
import Domain.Blog
import Domain.Domain
import Domain.Post
import Domain.Profile
import qualified Interpreter.HugoUtil as Hugo
import Interpreter.ParseBlogContent
import Interpreter.SerializeBlogContent (serializeBlogContent)
import qualified System.Directory as SD
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe), createProcess, shell, std_err, waitForProcess)
import Data.Either (fromRight)

data GitHugoEnv = GitHugoEnv
    { hugoConfigFilePath :: FilePath
    , hugoAuthorImageFilePath :: FilePath
    , hugoAuthorBioFilePath :: FilePath
    , hugoAuthorBioImagesDirectory :: FilePath
    , hugoPostsDirectory :: FilePath
    , hugoPostsImagesDirectory :: FilePath
    , hugoPostsCreationSorted :: MVar [PostID]
    }

{- | After invoking a shell command, this function prints stdout/err contents along with the exit code to stdout.
   Further, a textual http response is sent of stderr stream.
-}
handleShellError ::
    (MonadIO m) =>
    -- | StdOut handle
    Maybe Handle ->
    -- | StdErr handle
    Maybe Handle ->
    -- | Exitcode
    ExitCode ->
    m ()
handleShellError mstderr mstdout exit = do
    err <- maybe (pure "") hGetContents mstderr
    out <- maybe (pure "") hGetContents mstdout
    putStrLn $ "error! shell exit code " <> tshow exit
    print $ "error! stdout: " <> out
    print $ "error! stderr: " <> err

renewCreationDateSortedPosts :: (MonadIO m, MonadError Error m) => MVar [PostID] -> FilePath -> m ()
renewCreationDateSortedPosts mv postsDir = do
    _ <- takeMVar mv
    dirs <-
        liftIO $
            filterM (\d -> SD.doesFileExist $ postsDir </> d </> "index" <.> "md")
                =<< filterM (\d -> SD.doesDirectoryExist (postsDir </> d))
                =<< SD.listDirectory postsDir
    r <- forM dirs $ \d -> do
        bs <- readFile (postsDir </> d </> "index" <.> "md")
        case parseYamlFrontmatter bs of
            Done _ (PostFrontmatter inf) -> do
                pure $ Right (postInfoCreatedAt inf, d)
            _ -> do
                pure $ Left $ "failed parsing post frontmatter of post " <> d
    let (failed, available) = partitionEithers r
    forM_ failed $ putStrLn . pack
    putMVar mv $
        map (pack . snd) $
            sortBy (\(a, _) (b, _) -> compare b a) available -- compare reverse such that oldest posts are last.

defaultGitHugoEnv :: (MonadIO m) => m GitHugoEnv
defaultGitHugoEnv = do
    let defaultPostsDir = "content" </> "posts"
    sortedPosts <- newMVar []
    a <- runExceptT $ renewCreationDateSortedPosts sortedPosts defaultPostsDir
    case a of
        Left e -> do
            putStrLn $ "Error occured during startup of git/hugo: " <> tshow e
            liftIO exitFailure
        _ -> pure ()
    pure $
        GitHugoEnv
            { hugoConfigFilePath = "config" <.> "json"
            , hugoAuthorImageFilePath = "static" </> "img" </> "author" <.> "jpg"
            , hugoAuthorBioFilePath = "content" </> "about" </> "me" <.> "md"
            , hugoAuthorBioImagesDirectory = "static" </> "img" </> "bio"
            , hugoPostsDirectory = defaultPostsDir
            , hugoPostsImagesDirectory = "static" </> "img" </> "posts"
            , hugoPostsCreationSorted = sortedPosts
            }

type App m = (MonadReader GitHugoEnv m, MonadIO m, MonadError Error m, Monad m)

refreshPostsSorting :: (MonadReader GitHugoEnv m, MonadIO m, MonadError Error m) => m ()
refreshPostsSorting = do
    old <- asks hugoPostsCreationSorted
    postsDir <- asks hugoPostsDirectory
    renewCreationDateSortedPosts old postsDir

eval :: App m => Domain t -> m t
eval (Pure f) = pure f
eval (Bind da aToB) = eval da >>= eval . aToB
eval GetCurrentTime = liftIO getCurrentTime
eval Publish = do
    (_mstdin, mstdout, mstderr, phandle) <-
        liftIO $
            createProcess $
                (shell "git add . && git commit -m 'updated content' && git push && hugo && true")
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    }
    exit <- liftIO $ waitForProcess phandle
    if exit /= ExitSuccess
        then do
            handleShellError mstderr mstdout exit
            throwError $ ErrorUnknown "shell error publishing site."
        else pure ()
eval Revert = do
    (_mstdin, mstdout, mstderr, phandle) <-
        liftIO $
            createProcess $
                (shell "git restore . && git clean -df")
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    }
    exit <- liftIO $ waitForProcess phandle
    refreshPostsSorting
    if exit /= ExitSuccess
        then do
            handleShellError mstderr mstdout exit
            throwError $ ErrorUnknown "shell error cleaning changes."
        else pure ()
eval GetChanges = do
    (_mstdin, mstdout, mstderr, phandle) <-
        liftIO $
            createProcess $
                (shell "git status -s")
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    }
    exit <- liftIO $ waitForProcess phandle
    if exit /= ExitSuccess
        then do
            handleShellError mstderr mstdout exit
            throwError $ ErrorUnknown "shell error retrieving changes."
        else case mstdout of
            Just h -> do
                out <- hGetContents h
                if null out
                    then pure []
                    else pure $ lines $ decodeUtf8 out
            _ -> throwError $ ErrorUnknown ""
eval (SaveBlogInfo BlogInfo{blogHeader = h, blogMotto = m, blogBaseURL=burl, blogFooterText=f}) = do
    cfgFilePath <- asks hugoConfigFilePath
    let go act = first ErrorUnknown <$> runReaderT act cfgFilePath
    liftEither =<< go (runExceptT $ do
            ExceptT $ Hugo.writeHeader h 
            ExceptT $ Hugo.writeMotto m
            ExceptT $ Hugo.writeBaseURL burl
            ExceptT $ Hugo.writeFooterText f
        )
eval GetBlogInfo = do
    cfgFilePath <- asks hugoConfigFilePath
    let go act = first ErrorUnknown <$> runReaderT act cfgFilePath
    h <- go Hugo.readHeader
    m <- go Hugo.readMotto
    footerText <- go Hugo.readFooterText
    url <- go Hugo.readBaseURL
    liftEither $ BlogInfo <$> h <*> m <*> footerText <*> url
eval GetAuthorProfile = do
    cfgFilePath <- asks hugoConfigFilePath
    authorImgPath <- asks hugoAuthorImageFilePath
    let go act = first ErrorUnknown <$> runReaderT act cfgFilePath
    bio <- readBio
    authn <- go Hugo.readAuthorName
    authInsta <- go Hugo.readAuthorInstagram 
    pure $
        AuthorProfile
            (fromRight "unavailable" authn)
            (fromRight "unavailable" authInsta)
            bio
            (Image authorImgPath)
eval (SaveAuthorProfile AuthorProfile{authorName=a, authorInsta=i, authorBio = b}) = do
    cfgFilePath <- asks hugoConfigFilePath
    let go act = first ErrorUnknown <$> runReaderT act cfgFilePath
    _ <- go $ Hugo.writeAuthorName a
    _ <- go $ Hugo.writeAuthorInstagram i
    saveBio b
eval (UpdateAuthorAvatar bs) = do
    authorImgPath <- asks hugoAuthorImageFilePath
    let img = first (ErrorUnknown . pack) $ JP.decodeImage bs
        em = JP.saveJpgImage 100 authorImgPath <$> img
    case em of
        Left err -> throwError err
        Right d -> do
            liftIO d
            pure $ Image authorImgPath
eval (AddBioImage bs) = do
    bioImgsDir <- asks hugoAuthorBioImagesDirectory
    newImageFileName <- getSomeNewFileNameForDir bioImgsDir "jpg"
    let newImageFilePath = bioImgsDir </> newImageFileName
    case JP.decodeImage bs of
        Left err -> throwError $ ErrorUnknown $ pack err
        Right im -> do
            liftIO $ JP.saveJpgImage 100 newImageFilePath im
            pure $ Image newImageFilePath
eval (GetPostPages PostPagination{postPaginationLimit = lim}) = do
    ps <- asks hugoPostsCreationSorted >>= readMVar
    let (pages, rm) = length ps `quotRem` lim
    pure (pages + (if rm == 0 then 0 else 1))
eval (FindPosts PostPagination{postPaginationLimit = lim, postPaginationOffset = off}) = do
    ps <- asks hugoPostsCreationSorted >>= readMVar
    pure $ take lim $ drop (off * lim) ps
eval (GetPost pid) = do
    postsDir <- asks hugoPostsDirectory
    bs <- readFile (postsDir </> unpack pid </> "index" <.> "md")
    case parseYamlFrontmatter bs of
        Done bs' (PostFrontmatter postInformation) -> do
            if null bs' then 
                -- post content is empty, therefore there are no entries.
                pure $ Post pid postInformation []
            else do
                let content = first (ErrorUnknown . pack) (parseBlogContent bs')
                liftEither $ Post pid postInformation <$> content
        _ -> throwError $ ErrorUnknown $ "failed parsing front matter of post " <> pid
eval (NextPostID t) = do
    postsDir <- asks hugoPostsDirectory
    postID <- disambiguateFileNameForDir postsDir (dirNameForTitle t) ""
    pure $ pack postID
eval (AddPostImage pid bs) = do
    postsImgDir <- asks hugoPostsImagesDirectory
    liftIO $ SD.createDirectoryIfMissing False (postsImgDir </> unpack pid)
    imgFilePath <- (\f -> postsImgDir </> unpack pid </> f) <$> getSomeNewFileNameForDir (postsImgDir </> unpack pid) "jpg"
    case JP.decodeImage bs of
        Left err -> throwError $ ErrorUnknown $ pack err
        Right im -> do
            liftIO $ JP.saveJpgImage 100 imgFilePath im
            pure $ Image imgFilePath
eval (SavePost p) = do
    let pid = postId p
    postsDir <- asks hugoPostsDirectory
    author <- eval GetAuthorProfile
    let frontmatter = toStrict $ encode1 $ postFrontmatterToJson author (PostFrontmatter (postInfo p))
    liftIO $ SD.createDirectoryIfMissing False (postsDir </> unpack pid)
    writeFile (postsDir </> unpack pid </> "index" <.> "md") $
        "---\n"
            <> frontmatter
            <> "---\n"
            <> serializeBlogContent (postImgPublicPath pid) (postContent p)
    refreshPostsSorting -- TODO: make this concurrent somehow
eval (DeletePost pid) = do
    postsDir <- asks hugoPostsDirectory
    postsImgDir <- asks hugoPostsImagesDirectory
    liftIO $ SD.removeDirectoryRecursive (postsDir </> unpack pid)
    liftIO $ SD.removeDirectoryRecursive (postsImgDir </> unpack pid)
    sortedPosts <- asks hugoPostsCreationSorted
    liftIO $ modifyMVar_ sortedPosts $ \ps -> pure $ filter (/= pid) ps

dirNameForTitle :: PostTitle -> FilePath
dirNameForTitle =
    foldr
        ( \t acc ->
            ( case t of
                'ä' -> "ae"
                'Ä' -> "Ae"
                'ö' -> "oe"
                'ü' -> "ue"
                'Ö' -> "Oe"
                'Ü' -> "Ue"
                'ß' -> "ss"
                x ->
                    ( if x `elem` ['!', '.', ',', '?']
                        then ""
                        else
                            if isAlphaNum x
                                then [x]
                                else "-"
                    )
            )
                <> acc
        )
        ""

saveBio :: App m => BlogContent -> m ()
saveBio cnt = do
    bioFilePath <- asks hugoAuthorBioFilePath
    bs <- readFile bioFilePath
    now <- eval GetCurrentTime
    case parseYamlFrontmatter bs of
        Done _ bfm -> do
            writeFile bioFilePath $
                "---\n"
                    <> toStrict (encode1 (bfm{bioFrontmatterLastMod = now}))
                    <> "---\n"
                    <> serializeBlogContent bioImgPublicPath cnt
        _ -> throwError $ ErrorUnknown "failed parsing front matter of bio."

readBio :: App m => m BlogContent
readBio = do
    bioFilePath <- asks hugoAuthorBioFilePath
    bs <- readFile bioFilePath
    case parseYamlFrontmatter bs of
        Done bs' _b@BioFrontmatter{} -> do
            let content = first (ErrorUnknown . pack) (parseBlogContent bs')
            liftEither content
        _ -> throwError $ ErrorUnknown "failed parsing front matter of bio."

postImgPublicPath :: PostID -> FilePath -> Text
postImgPublicPath _pid imgName = dropPrefix (pack $ "static" </> "") $ pack imgName

bioImgPublicPath :: FilePath -> Text
bioImgPublicPath imgName = dropPrefix (pack $ "static" </> "") $ pack imgName

getSomeNewFileNameForDir ::
    App m =>
    -- | The directory to search in
    FilePath ->
    -- | The file extension
    Text ->
    m FilePath
getSomeNewFileNameForDir dir ext = do
    dirConts <- liftIO $ SD.listDirectory dir
    let possibleFiles = map (\d -> show d <.> unpack ext) ([1 ..] :: [Integer])
        (a : _) = filter (`notElem` dirConts) possibleFiles
    pure a

disambiguateFileNameForDir ::
    App m =>
    FilePath ->
    FilePath ->
    FilePath ->
    m FilePath
disambiguateFileNameForDir dir fn ext = do
    dirConts <- liftIO $ SD.listDirectory dir
    let possibleFiles = map (\d -> fn <> d <.> unpack ext) ("" : map (\d -> '-' : show d) ([2 ..] :: [Integer]))
        (a : _) = filter (`notElem` dirConts) possibleFiles
    pure a

data BioFrontmatter = BioFrontmatter
    { bioFrontmatterCreatedAt :: UTCTime
    , bioFrontmatterLastMod :: UTCTime
    , bioFrontmatterTitle :: Text
    }
instance FromJSON BioFrontmatter where
    parseJSON (Object v) =
        BioFrontmatter
            <$> v .: "date"
            <*> v .: "lastmod"
            <*> v .: "title"
    parseJSON _ = fail "not a json object!"
instance ToJSON BioFrontmatter where
    toJSON (BioFrontmatter ca lm t) =
        object
            [ "date" .= ca
            , "lastmod" .= lm
            , "title" .= t
            , "draft" .= False
            ]

newtype PostFrontmatter = PostFrontmatter PostInformation
instance FromJSON PostFrontmatter where
    parseJSON (Object v) = do
        inf <-
            PostInformation
                <$> v .: "id"
                <*> v .: "lastmod"
                <*> v .: "date"
                <*> (Image <$> (v .: "staticCover"))
                <*> v .: "tags"
                <*> v .: "categories"
                <*> v .: "title"
        pure $ PostFrontmatter inf
    parseJSON _ = fail "postInformation is not an object."


postFrontmatterToJson :: AuthorProfile -> PostFrontmatter -> Value
postFrontmatterToJson AuthorProfile{authorName=n, authorInsta=i} ( PostFrontmatter
                PostInformation
                    { postInfoID = pid
                    , postInfoCategories = cats
                    , postInfoCoverImage = (Image pth)
                    , postInfoCreatedAt = createdAt
                    , postInfoLastMod = lastMod
                    , postInfoTags = tags
                    , postInfoTitle = title
                    }
            ) =
    object
                [ "id" .= pid
                , "categories" .= cats
                , "staticCover" .= pth
                , "cover" .= postImgPublicPath pid pth
                , "date" .= createdAt
                , "lastmod" .= lastMod
                , "tags" .= tags
                , "title" .= title
                , "draft" .= False
                , "author" .= n
                , "avatar" .= ("/img/author.jpg" :: Text)
                , "authorLink" .= ("https://instagram.com/" <> i)
                ]
