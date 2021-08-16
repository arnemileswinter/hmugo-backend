{-# LANGUAGE LambdaCase #-}

module Application.Rest where

import Application.JsonTypes
import Application.RestUtil
import ClassyPrelude
import Control.Monad.Except (runExceptT)
import qualified Data.Text.Lazy as TL
import qualified Domain.BlogDomain as BlogDomain
import Domain.Domain (Domain, Error (ErrorNotFound, ErrorUnknown))
import Domain.Post (Post (postId, postInfo), PostInformation (postInfoTitle), PostPagination (..))
import qualified Domain.PostDomain as PostDomain
import qualified Domain.ProfileDomain as ProfileDomain
import Env (Env (envApiKey), getEnvIO)
import Interpreter.GitHugoInterpreter (GitHugoEnv, defaultGitHugoEnv, eval)
import Network.HTTP.Types (status200, status204, status404, status500)
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (ActionT, ScottyError, ScottyT, finish, get, json, jsonData, middleware, param, post, put, rescue, scottyT, status, text)
import qualified Web.Scotty.Trans as S

-- | Retrieve a request query parameter boxed in a maybe.
maybeParam :: (ScottyError e, Monad m, S.Parsable a) => TL.Text -> ActionT e m (Maybe a)
maybeParam k = (Just <$> param k) `rescue` (\_ -> pure Nothing)

-- | Retrieve pagination limit and offset, according to query params limit and offset, yielding defaults
optionalPostPagination :: (ScottyError e, Monad m) => ActionT e m PostPagination
optionalPostPagination = do
    limit <- maybeParam "limit"
    offset <- maybeParam "offset"
    pure $ go limit offset
  where
    defaultLimit = 20
    go Nothing Nothing = PostPagination{postPaginationLimit = defaultLimit, postPaginationOffset = 0}
    go Nothing (Just o) = PostPagination{postPaginationLimit = defaultLimit, postPaginationOffset = o}
    go (Just 0) Nothing = PostPagination{postPaginationLimit = defaultLimit, postPaginationOffset = 0}
    go (Just l) Nothing = PostPagination{postPaginationLimit = l, postPaginationOffset = 0}
    go (Just l) (Just o) = PostPagination{postPaginationLimit = l, postPaginationOffset = o}

routes :: (MonadIO m, Monad m, MonadReader GitHugoEnv m) => ScottyT TL.Text m ()
routes = do
    get "/v1/blog" $ do
        bi <- invoke BlogDomain.getBlogInfo
        jbi <- jsonBlogInfo bi
        json jbi
    put "/v1/blog" $ do
        (JsonUpdateBlogInfo up) <- jsonData
        _ <- invoke $ BlogDomain.updateBlogInfo up
        status status204

    get "/v1/blog/changes" $ do
        chs <- invoke BlogDomain.getChanges
        if null chs
            then status status204
            else json chs
    S.delete "/v1/blog/changes" $ do
        invoke BlogDomain.revert
        status status204
    post "/v1/blog/publish" $ do
        invoke BlogDomain.publish
        status status204

    get "/v1/profile" $ do
        p <- invoke ProfileDomain.getAuthorProfile
        p' <- jsonAuthorProfile p
        json p'
    put "/v1/profile" $ do
        (JsonUpdateAuthorProfile up) <- jsonData
        _ <- invoke $ ProfileDomain.updateAuthorProfile up
        status status204

    get "/v1/posts" $ do
        pg <- optionalPostPagination
        i <- invoke $ PostDomain.getPages pg
        ps <- invoke $ PostDomain.findPosts pg
        ps' <- forM ps $ \pid -> do
            p <- invoke $ PostDomain.getPost pid
            pure (postId p, postInfoTitle $ postInfo p)
        json $ jsonPaginatedPosts i ps'
    post "/v1/posts" $ do
        (JsonCreatePost c) <- jsonData
        p <- invoke $ PostDomain.createPost c
        jp <- jsonPost p
        json jp
    get "/v1/posts/:pid" $ do
        pid <- param "pid"
        p <- invoke $ PostDomain.getPost pid
        jp <- jsonPost p
        json jp
    S.delete "/v1/posts/:pid" $ do
        pid <- param "pid"
        invoke $ PostDomain.deletePost pid
        status status200
    put "/v1/posts/:pid" $ do
        pid <- param "pid"
        (JsonUpdatePost up) <- jsonData
        _ <- invoke $ PostDomain.updatePost pid up
        status status204

handleDomainError :: (Monad m, MonadIO m, ScottyError e) => Error -> ActionT e m ()
handleDomainError (ErrorUnknown a) = do
    putStrLn ("Unexpected error!" <> a)
    status status500
handleDomainError (ErrorNotFound w) = do
    text (fromStrict $ "not found. " <> w)
    status status404

invoke :: (ScottyError e, MonadReader GitHugoEnv m, MonadIO m) => Domain a -> ActionT e m a
invoke d =
    runExceptT (eval d) >>= \case
        Left a -> handleDomainError a >> finish
        Right a -> pure a

main :: IO ()
main = do
    env <- getEnvIO
    ghEnv <- defaultGitHugoEnv
    scottyT 80 (`runReaderT` ghEnv) $ do
        middleware realIp
        middleware withCors
        middleware logStdout
        middleware $ withApiKeyHeader (envApiKey env)

        routes
