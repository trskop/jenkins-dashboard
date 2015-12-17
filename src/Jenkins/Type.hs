{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Jenkins.Type where

import GHC.Generics (Generic)


import Control.Applicative ((<*>), (<|>), pure)
import Control.Exception (IOException, handle)
import Control.Monad ((>>=), (>>), return, fail)
import Data.Bool (Bool, (&&))
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Ord (Ord)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show, show)
import System.IO (IO)

import Data.Text (Text, unpack)

import Data.Aeson
    ( FromJSON(parseJSON)
    , (.:)
    , eitherDecode
    , withObject
    , withText
    )
import Data.Aeson.Types (Parser)

import qualified Data.Attoparsec.Text as A
    ( Parser
    , parseOnly
    , string
    )

import Network.HTTP
    ( HeaderName(HdrAuthorization)
    , Request_String
    , getRequest
    , getResponseBody
    , insertHeader
    , simpleHTTP
    )
import Network.HTTP.Auth
    ( Authority (AuthBasic)
    , withAuthority
    )
import Network.URI (nullURI)


data Activity = Building | Idle
  deriving (Show, Generic, Typeable, Eq, Ord)

data LastRun =
      Aborted
    | Disabled
    | Failed
    | NotBuilded
    | Success
    | Unstable
  deriving (Show, Generic, Typeable, Eq, Ord)

data JobStatus = JobStatus
    { lastRun :: LastRun
    , activity :: Activity
    }
  deriving (Show, Generic, Typeable, Eq, Ord)

instance FromJSON JobStatus where
    parseJSON = withText "JobStatus" parseStatus

data Job = Job
    { name :: String
    , url :: String
    , status :: JobStatus
    }
  deriving (Show, Generic, Typeable, Eq, Ord)

instance FromJSON Job where
    parseJSON = withObject "Job" $ \o -> Job
        <$> o .: "name"
        <*> o .: "url"
        <*> o .: "color"

newtype Jobs = Jobs { getJobs :: [Job] }
  deriving (Show, Generic, Typeable, Eq, Ord)

instance FromJSON Jobs where
    parseJSON = withObject "Jobs" $ \o ->  Jobs <$> o .: "jobs"

statusParser :: A.Parser JobStatus
statusParser =
        JobStatus Success <$> (A.string "blue" >> activityParser)
    <|> JobStatus Unstable <$> (A.string "yellow" >> activityParser)
    <|> JobStatus Failed <$> (A.string "red" >> activityParser)
    <|> JobStatus Aborted <$> (A.string "aborted" >> activityParser)
    <|> JobStatus Disabled <$> (A.string "disabled" >> activityParser)
    <|> JobStatus NotBuilded  <$> (A.string "notbuilt" >> activityParser)

activityParser :: A.Parser Activity
activityParser =
    (A.string "_anime" >> return Building)
    <|> return Idle

parseStatus :: Text -> Parser JobStatus
parseStatus s =
    case A.parseOnly statusParser s of
        Left _ -> fail . unpack
            $ "can't parse JobStatus; parsed object: " <>  s
        Right r -> pure r

getJenkinsJobs
    :: Maybe (String, String)
    -- ^ Tuple with name and password (token).
    -> String
    -- ^ Jenkins server url.
    -> IO (Either String Jobs)
    -- ^ On success returns Righ Jobs. Jobs contain state and name of each job
    -- on server.
getJenkinsJobs authData url' =
    handle handler (eitherDecode . C.pack <$> getJenkinsJson)
  where
    handler :: IOException -> IO (Either String Jobs)
    handler e = return . Left $ show e

    getJenkinsJson :: IO String
    getJenkinsJson = (simpleHTTP . auth . getRequest $ url' <> "/api/json")
        >>= getResponseBody

    auth :: Request_String -> Request_String
    auth r = maybe r f authData where
        f (usr, pas) = insertHeader HdrAuthorization
            (withAuthority (AuthBasic ("" :: String) usr pas nullURI) r) r

jobLastRun :: Job -> LastRun
jobLastRun = lastRun . status

jobActivity :: Job -> Activity
jobActivity = activity . status

recentlyFailed :: Job -> Bool
recentlyFailed = ((&&) . failed) <*> finished where
    failed = (Failed ==) . lastRun . status
    finished =  (Idle ==) . activity . status
