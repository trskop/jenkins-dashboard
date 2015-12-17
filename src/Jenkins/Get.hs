{-# LANGUAGE NoImplicitPrelude #-}
module Jenkins.Get
    ( getJenkinsJobs
    ) where

import Control.Exception (IOException, handle)
import Control.Monad (Monad((>>=), return))
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import Data.Either (Either(Left))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO)
import Text.Show(Show(show))

import Data.Aeson (eitherDecode)
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

import Jenkins.Type (Jobs)


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

