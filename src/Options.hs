{-# LANGUAGE NoImplicitPrelude #-}
module Options
    ( configOptions
    ) where

import Control.Applicative
    ( Alternative((<|>))
    , Applicative((<*>))
    , many
    )
import Control.Arrow (second)
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.String (String)

import Options.Applicative
    ( ReadM
    , eitherReader
    , Parser
    , strOption
    , long
    , metavar
    , help
    , optional
    , option
    , long
    , metavar
    , help
    , pure
    , option
    , auto
    , long
    , metavar
    , help
    )

import Type (Config(Config))

parseCredentials :: ReadM (String, String)
parseCredentials = eitherReader parseHostPort'
    where
    parseHostPort' s
        | isEmpty s          = Left argumentEmpty
        | List.notElem ':' s = Left unexpectedFormat
        | otherwise          = Right $ split s
      where
        isEmpty = List.null
        split = second List.tail . List.break (== ':')
        argumentEmpty = "Argument USERNAME:API_COTKEN can not be empty."
        unexpectedFormat = "Argument has to be in USERNAME:API_COTKEN format."


configOptions :: Parser Config
configOptions = Config
     <$> strOption
        (long "jenkins"
        <> metavar "URL"
        <> help "Jenkins url, including protocol")
     <*> optional
        (option parseCredentials $ long "credentials"
        <> metavar "USER_NAME:API_TOKEN"
        <> help "Credentials to access Jenkins")
     <*> (<|> pure 1000000)
        (option auto $ long "delay"
        <> metavar "NANOSECONDS"
        <> help "Delay between two fetches")
    <*> (many . strOption)
        (long "ignore"
        <> metavar "NAME"
        <> help "Job name to ignore")


