{-# LANGUAGE NoImplicitPrelude #-}
module Type where

import Control.Monad.Reader (ReaderT)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import System.IO (IO)
import Text.Show (Show)


data Config = Config
    { url :: String
    , credentials:: Maybe (String, String)
    , refreshDelay :: Int
    } deriving Show

type M = ReaderT Config IO
