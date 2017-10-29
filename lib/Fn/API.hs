{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Fn.API
  -- * Client and Server
  ( ServerConfig(..)
  , FnBackend
  , createFnClient
  , runFnServer
  , runFnClient
  , runFnClientWithManager
  , FnClient
  -- ** Servant
  , FnAPI
  ) where

import Fn.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData

-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for Fn.
type FnAPI
    =    "apps" :> Capture "app" Text :> Verb 'DELETE 200 '[JSON] () -- 'appsAppDelete' route
    :<|> "apps" :> Capture "app" Text :> Verb 'GET 200 '[JSON] AppWrapper -- 'appsAppGet' route
    :<|> "apps" :> Capture "app" Text :> ReqBody '[JSON] AppWrapper :> Verb 'PATCH 200 '[JSON] AppWrapper -- 'appsAppPatch' route
    :<|> "apps" :> QueryParam "cursor" Text :> QueryParam "per_page" Int :> Verb 'GET 200 '[JSON] AppsWrapper -- 'appsGet' route
    :<|> "apps" :> ReqBody '[JSON] AppWrapper :> Verb 'POST 200 '[JSON] AppWrapper -- 'appsPost' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> Capture "call" Text :> Verb 'GET 200 '[JSON] CallWrapper -- 'appsAppCallsCallGet' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> Capture "call" Text :> "log" :> Verb 'DELETE 200 '[JSON] () -- 'appsAppCallsCallLogDelete' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> Capture "call" Text :> "log" :> Verb 'GET 200 '[JSON] LogWrapper -- 'appsAppCallsCallLogGet' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> QueryParam "path" Text :> QueryParam "cursor" Text :> QueryParam "per_page" Int :> QueryParam "from_time" Int :> QueryParam "to_time" Int :> Verb 'GET 200 '[JSON] CallsWrapper -- 'appsAppCallsGet' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> Capture "call" Text :> "log" :> Verb 'DELETE 200 '[JSON] () -- 'appsAppCallsCallLogDelete' route
    :<|> "apps" :> Capture "app" Text :> "calls" :> Capture "call" Text :> "log" :> Verb 'GET 200 '[JSON] LogWrapper -- 'appsAppCallsCallLogGet' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> QueryParam "image" Text :> QueryParam "cursor" Text :> QueryParam "per_page" Int :> Verb 'GET 200 '[JSON] RoutesWrapper -- 'appsAppRoutesGet' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> ReqBody '[JSON] RouteWrapper :> Verb 'POST 200 '[JSON] RouteWrapper -- 'appsAppRoutesPost' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> Capture "route" Text :> Verb 'DELETE 200 '[JSON] () -- 'appsAppRoutesRouteDelete' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> Capture "route" Text :> Verb 'GET 200 '[JSON] RouteWrapper -- 'appsAppRoutesRouteGet' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> Capture "route" Text :> ReqBody '[JSON] RouteWrapper :> Verb 'PATCH 200 '[JSON] RouteWrapper -- 'appsAppRoutesRoutePatch' route
    :<|> "apps" :> Capture "app" Text :> "routes" :> Capture "route" Text :> ReqBody '[JSON] RouteWrapper :> Verb 'PUT 200 '[JSON] RouteWrapper -- 'appsAppRoutesRoutePut' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for Fn.
-- The backend can be used both for the client and the server. The client generated from the Fn Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createFnClient@). Alternatively, provided
-- a backend, the API can be served using @runFnServer@.
data FnBackend m = FnBackend
  { appsAppDelete :: Text -> m (){- ^ Delete an app. -}
  , appsAppGet :: Text -> m AppWrapper{- ^ This gives more details about a app, such as statistics. -}
  , appsAppPatch :: Text -> AppWrapper -> m AppWrapper{- ^ You can set app level settings here.  -}
  , appsGet :: Maybe Text -> Maybe Int -> m AppsWrapper{- ^ Get a list of all the apps in the system, returned in alphabetical order. -}
  , appsPost :: AppWrapper -> m AppWrapper{- ^ Insert a new app -}
  , appsAppCallsCallGet :: Text -> Text -> m CallWrapper{- ^ Get call information -}
  , appsAppCallsCallLogDelete :: Text -> Text -> m (){- ^ Delete call log entry -}
  , appsAppCallsCallLogGet :: Text -> Text -> m LogWrapper{- ^ Get call logs -}
  , appsAppCallsGet :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> m CallsWrapper{- ^ Get app-bound calls can filter to route-bound calls, results returned in created_at, descending order (newest first). -}
  , appsAppCallsCallLogDelete :: Text -> Text -> m (){- ^ Delete call log entry -}
  , appsAppCallsCallLogGet :: Text -> Text -> m LogWrapper{- ^ Get call logs -}
  , appsAppRoutesGet :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> m RoutesWrapper{- ^ This will list routes for a particular app, returned in alphabetical order. -}
  , appsAppRoutesPost :: Text -> RouteWrapper -> m RouteWrapper{- ^ Create a new route in an app, if app doesn't exists, it creates the app. Post does not skip validation of zero values. -}
  , appsAppRoutesRouteDelete :: Text -> Text -> m (){- ^ Deletes the route. -}
  , appsAppRoutesRouteGet :: Text -> Text -> m RouteWrapper{- ^ Gets a route by name. -}
  , appsAppRoutesRoutePatch :: Text -> Text -> RouteWrapper -> m RouteWrapper{- ^ Update a route -}
  , appsAppRoutesRoutePut :: Text -> Text -> RouteWrapper -> m RouteWrapper{- ^ Update or Create a route -}
  }

newtype FnClient a = FnClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative FnClient where
  pure x = FnClient (\_ _ -> pure x)
  (FnClient f) <*> (FnClient x) =
    FnClient (\manager url -> f manager url <*> x manager url)

instance Monad FnClient where
  (FnClient a) >>= f =
    FnClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO FnClient where
  liftIO io = FnClient (\_ _ -> liftIO io)

createFnClient :: FnBackend FnClient
createFnClient = FnBackend{..}
  where
    ((coerce -> appsAppDelete) :<|>
     (coerce -> appsAppGet) :<|>
     (coerce -> appsAppPatch) :<|>
     (coerce -> appsGet) :<|>
     (coerce -> appsPost) :<|>
     (coerce -> appsAppCallsCallGet) :<|>
     (coerce -> appsAppCallsCallLogDelete) :<|>
     (coerce -> appsAppCallsCallLogGet) :<|>
     (coerce -> appsAppCallsGet) :<|>
     (coerce -> appsAppCallsCallLogDelete) :<|>
     (coerce -> appsAppCallsCallLogGet) :<|>
     (coerce -> appsAppRoutesGet) :<|>
     (coerce -> appsAppRoutesPost) :<|>
     (coerce -> appsAppRoutesRouteDelete) :<|>
     (coerce -> appsAppRoutesRouteGet) :<|>
     (coerce -> appsAppRoutesRoutePatch) :<|>
     (coerce -> appsAppRoutesRoutePut)) = client (Proxy :: Proxy FnAPI)

-- | Run requests in the FnClient monad.
runFnClient :: ServerConfig -> FnClient a -> ExceptT ServantError IO a
runFnClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runFnClientWithManager manager clientConfig cl

-- | Run requests in the FnClient monad using a custom manager.
runFnClientWithManager :: Manager -> ServerConfig -> FnClient a -> ExceptT ServantError IO a
runFnClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the Fn server at the provided host and port.
runFnServer :: MonadIO m => ServerConfig -> FnBackend (ExceptT ServantErr IO)  -> m ()
runFnServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy FnAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend FnBackend{..} =
      (coerce appsAppDelete :<|>
       coerce appsAppGet :<|>
       coerce appsAppPatch :<|>
       coerce appsGet :<|>
       coerce appsPost :<|>
       coerce appsAppCallsCallGet :<|>
       coerce appsAppCallsCallLogDelete :<|>
       coerce appsAppCallsCallLogGet :<|>
       coerce appsAppCallsGet :<|>
       coerce appsAppCallsCallLogDelete :<|>
       coerce appsAppCallsCallLogGet :<|>
       coerce appsAppRoutesGet :<|>
       coerce appsAppRoutesPost :<|>
       coerce appsAppRoutesRouteDelete :<|>
       coerce appsAppRoutesRouteGet :<|>
       coerce appsAppRoutesRoutePatch :<|>
       coerce appsAppRoutesRoutePut)
