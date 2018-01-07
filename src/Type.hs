{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Type where

import Data.Functor (fmap)
import Data.Function (($), (&), on)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String, fromString)
import Data.Tuple (uncurry)
import System.IO (IO)
import Text.Show (Show)

import Control.Lens ((.~))
import Control.Monad.Reader (ReaderT)
import qualified Network.Wreq as Wreq (Options, auth, basicAuth, defaults)


data Config = Config
    { wreqOptions :: !Wreq.Options
    , url :: !String
    , credentials :: !(Maybe (String, String))
    , refreshDelay :: !Int
    , boringNames :: ![String]
    } deriving Show

type M = ReaderT Config IO

mkConfig
    :: String
    -> Maybe (String, String)
    -> Int
    -> [String]
    -> Config
mkConfig url credentials refreshDelay boringNames = Config{..}
  where
    wreqOptions = Wreq.defaults
        & Wreq.auth .~ basicAuth credentials
      where
        basicAuth = fmap $ uncurry (Wreq.basicAuth `on` fromString)
