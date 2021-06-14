module Platform.HTTP
      ( main
      ) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response, Application, Request, pathInfo)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors

import qualified Feature.Auth.HTTP as Auth
import qualified Feature.User.HTTP as User
import qualified Feature.Comment.HTTP as Comment
import qualified Feature.Article.HTTP as Article
import qualified Data.Map as M

import System.Environment

type App r m = (Article.Service m, Auth.Service m, Comment.Service m, User.Service m, MonadIO m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  mayTLSSetting <- acquireTLSSetting
  resq <- newIORef M.empty
  case mayTLSSetting of
    Nothing ->
      scottyT port runner (routes resq)
    Just tlsSetting -> do
      app <- scottyAppT runner (routes resq)
      runTLS tlsSetting (setPort port defaultSettings) app
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port
    acquireTLSSetting = do
      env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
      let enableHttps = fromMaybe True env
      return $ if enableHttps
        then Just $ tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
        else Nothing



-- * Routing

routes :: (App r m) => IORef (M.Map [Text] Int) -> ScottyT LText m ()
routes resp = do
  -- middlewares
  middleware $ requestCounter resp . (cors $ const $ Just simpleCorsResourcePolicy
    { corsRequestHeaders = "Authorization":simpleHeaders
    , corsMethods = "PUT":"DELETE":simpleMethods
    })
  options (regex ".*") $ return ()

  -- err
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  User.routes
  Article.routes
  Comment.routes

  -- health
  get "/api/health" $
    json True


requestCounter :: IORef (M.Map [Text] Int) -> Application -> Application
requestCounter size_ref app req resp = do
  modifyIORef size_ref (M.insertWith (+) (pathInfo req) 1)
  app req resp
