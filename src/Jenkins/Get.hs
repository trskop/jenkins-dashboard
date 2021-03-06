{-# LANGUAGE NoImplicitPrelude #-}
module Jenkins.Get
    ( getJenkinsJobs
    ) where

import Control.Applicative (pure)
import Control.Exception (IOException, handle)
import Data.Bool (otherwise)
import Data.Either (Either(Left))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO)
import Text.Show(show)

import Control.Lens ((^.), views)
import qualified Data.Aeson as Aeson (eitherDecode)
import qualified Network.Wreq as Wreq

import Jenkins.Type (Jobs)


getJenkinsJobs
    :: Wreq.Options
    -> String
    -- ^ Jenkins server url.
    -> IO (Either String Jobs)
    -- ^ On success returns Righ Jobs. Jobs contain state and name of each job
    -- on server.
getJenkinsJobs opts url = handle ioExceptionHandler
    $ on200Ok Aeson.eitherDecode <$> Wreq.getWith opts (url <> "/api/json")
  where
    ioExceptionHandler :: IOException -> IO (Either String Jobs)
    ioExceptionHandler e = pure . Left $ show e

    on200Ok f r
      | statusCode == 200 = views Wreq.responseBody f r
      | otherwise = failWithUnexpectedStatusCode
      where
        statusCode = r ^. Wreq.responseStatus . Wreq.statusCode

        failWithUnexpectedStatusCode =
            Left $ "Unexpected status code in response: " <> show statusCode
