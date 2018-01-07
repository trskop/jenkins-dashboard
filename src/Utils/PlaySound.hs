{-# LANGUAGE NoImplicitPrelude #-}
module Utils.PlaySound
    ( playSound
    ) where

import Control.Monad (void)
import Data.Bool (Bool(True))
import Data.Function (($), (.))
import Data.String (String)
import System.IO (FilePath, IO, IOMode(ReadWriteMode), withFile)
import System.Process
    ( CreateProcess
        ( std_in
        , std_out
        , std_err
        , close_fds
        )
    , StdStream(UseHandle)
    , createProcess
    , proc
    )


player :: FilePath
player = "mplayer"

playerOpts :: FilePath -> [String]
playerOpts x = ["-really-quiet", x]

playSound :: FilePath -> IO ()
playSound x = void . withFile "/dev/null" ReadWriteMode $ \ h ->
    createProcess $ (proc player (playerOpts x))
        { std_in = UseHandle h
        , std_out = UseHandle h
        , std_err = UseHandle h
        , close_fds = True
        }
