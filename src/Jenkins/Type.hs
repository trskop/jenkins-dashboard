{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Type where

import GHC.Generics (Generic)

import Control.Applicative ((*>), (<*>), (<|>), pure)
import Control.Monad (fail)
import Data.Bool (Bool, (&&))
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.List (elem)
import Data.Ord (Ord)
import Data.Function (($), (.))
import Data.Functor (($>), (<$>))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show)

import Data.Text (Text, unpack)

import Data.Aeson
    ( FromJSON(parseJSON)
    , (.:)
    , withObject
    , withText
    )
import Data.Aeson.Types (Parser)

import qualified Data.Attoparsec.Text as A
    ( Parser
    , parseOnly
    , string
    )


data Activity = Building | Idle
  deriving (Show, Generic, Typeable, Eq, Ord)

data LastRun
    = Aborted
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
    parseJSON = withObject "Jobs" $ \o -> Jobs <$> o .: "jobs"

statusParser :: A.Parser JobStatus
statusParser =
        JobStatus Success <$> (A.string "blue" *> activityParser)
    <|> JobStatus Unstable <$> (A.string "yellow" *> activityParser)
    <|> JobStatus Failed <$> (A.string "red" *> activityParser)
    <|> JobStatus Aborted <$> (A.string "aborted" *> activityParser)
    <|> JobStatus Disabled <$> (A.string "disabled" *> activityParser)
    <|> JobStatus NotBuilded <$> (A.string "notbuilt" *> activityParser)

activityParser :: A.Parser Activity
activityParser =
    (A.string "_anime" $> Building)
    <|> pure Idle

parseStatus :: Text -> Parser JobStatus
parseStatus s =
    case A.parseOnly statusParser s of
        Left _ -> fail . unpack
            $ "can't parse JobStatus; parsed object: " <> s
        Right r -> pure r

jobLastRun :: Job -> LastRun
jobLastRun = lastRun . status

jobActivity :: Job -> Activity
jobActivity = activity . status

failed :: Job -> Bool
failed = (Failed ==) . lastRun . status

failedOrUnstable :: Job -> Bool
failedOrUnstable = (`elem` [Failed, Unstable]) . lastRun . status

recentlyFailed :: Job -> Bool
recentlyFailed = ((&&) . failed) <*> finished where
    finished = (Idle ==) . activity . status
