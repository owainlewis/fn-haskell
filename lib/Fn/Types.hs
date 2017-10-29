{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Fn.Types (
  App (..),
  AppWrapper (..),
  AppsWrapper (..),
  Call (..),
  CallWrapper (..),
  CallsWrapper (..),
  Error (..),
  ErrorBody (..),
  Log (..),
  LogWrapper (..),
  Route (..),
  RouteWrapper (..),
  RoutesWrapper (..),
  Version (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data App = App
  { appName :: Text -- ^ Name of this app. Must be different than the image name. Can ony contain alphanumeric, -, and _.
  , appConfig :: Map.Map String Text -- ^ Application configuration, applied to all routes.
  } deriving (Show, Eq, Generic)

instance FromJSON App where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "app")
instance ToJSON App where
  toJSON = genericToJSON (removeFieldLabelPrefix False "app")

-- | 
data AppWrapper = AppWrapper
  { appWrapperApp :: App -- ^ 
  , appWrapperError :: ErrorBody -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AppWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appWrapper")
instance ToJSON AppWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appWrapper")

-- | 
data AppsWrapper = AppsWrapper
  { appsWrapperNext'Underscorecursor :: Text -- ^ cursor to send with subsequent request to receive the next page, if non-empty
  , appsWrapperApps :: [App] -- ^ 
  , appsWrapperError :: ErrorBody -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AppsWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appsWrapper")
instance ToJSON AppsWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appsWrapper")

-- | 
data Call = Call
  { callId :: Text -- ^ Call UUID ID.
  , callStatus :: Text -- ^ Call execution status.
  , callApp'Underscorename :: Text -- ^ App name that is assigned to a route that is being executed.
  , callPath :: Text -- ^ App route that is being executed.
  , callCreated'Underscoreat :: Integer -- ^ Time when call was submitted. Always in UTC.
  , callStarted'Underscoreat :: Integer -- ^ Time when call started execution. Always in UTC.
  , callCompleted'Underscoreat :: Integer -- ^ Time when call completed, whether it was successul or failed. Always in UTC.
  } deriving (Show, Eq, Generic)

instance FromJSON Call where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "call")
instance ToJSON Call where
  toJSON = genericToJSON (removeFieldLabelPrefix False "call")

-- | 
data CallWrapper = CallWrapper
  { callWrapperCall :: Call -- ^ Call object.
  } deriving (Show, Eq, Generic)

instance FromJSON CallWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "callWrapper")
instance ToJSON CallWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "callWrapper")

-- | 
data CallsWrapper = CallsWrapper
  { callsWrapperNext'Underscorecursor :: Text -- ^ cursor to send with subsequent request to receive the next page, if non-empty
  , callsWrapperCalls :: [Call] -- ^ 
  , callsWrapperError :: ErrorBody -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON CallsWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "callsWrapper")
instance ToJSON CallsWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "callsWrapper")

-- | 
data Error = Error
  { errorError :: ErrorBody -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")

-- | 
data ErrorBody = ErrorBody
  { errorBodyMessage :: Text -- ^ 
  , errorBodyFields :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorBody where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorBody")
instance ToJSON ErrorBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorBody")

-- | 
data Log = Log
  { logCall'Underscoreid :: Text -- ^ Call UUID ID
  , logLog :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Log where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "log")
instance ToJSON Log where
  toJSON = genericToJSON (removeFieldLabelPrefix False "log")

-- | 
data LogWrapper = LogWrapper
  { logWrapperLog :: Log -- ^ Call log entry.
  } deriving (Show, Eq, Generic)

instance FromJSON LogWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logWrapper")
instance ToJSON LogWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logWrapper")

-- | 
data Route = Route
  { routePath :: Text -- ^ URL path that will be matched to this route
  , routeImage :: Text -- ^ Name of Docker image to use in this route. You should include the image tag, which should be a version number, to be more accurate. Can be overridden on a per route basis with route.image.
  , routeHeaders :: Map.Map String [Text] -- ^ Map of http headers that will be sent with the response
  , routeMemory :: Int -- ^ Max usable memory for this route (MiB).
  , routeType :: Text -- ^ Route type
  , routeFormat :: Text -- ^ Payload format sent into function.
  , routeConfig :: Map.Map String Text -- ^ Route configuration - overrides application configuration
  , routeTimeout :: Int -- ^ Timeout for executions of this route. Value in Seconds
  , routeIdle'Underscoretimeout :: Int -- ^ Hot functions idle timeout before termination. Value in Seconds
  } deriving (Show, Eq, Generic)

instance FromJSON Route where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "route")
instance ToJSON Route where
  toJSON = genericToJSON (removeFieldLabelPrefix False "route")

-- | 
data RouteWrapper = RouteWrapper
  { routeWrapperMessage :: Text -- ^ 
  , routeWrapperError :: ErrorBody -- ^ 
  , routeWrapperRoute :: Route -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON RouteWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routeWrapper")
instance ToJSON RouteWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routeWrapper")

-- | 
data RoutesWrapper = RoutesWrapper
  { routesWrapperNext'Underscorecursor :: Text -- ^ cursor to send with subsequent request to receive the next page, if non-empty
  , routesWrapperRoutes :: [Route] -- ^ 
  , routesWrapperError :: ErrorBody -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutesWrapper where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routesWrapper")
instance ToJSON RoutesWrapper where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routesWrapper")

-- | 
data Version = Version
  { versionVersion :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "version")
instance ToJSON Version where
  toJSON = genericToJSON (removeFieldLabelPrefix False "version")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
