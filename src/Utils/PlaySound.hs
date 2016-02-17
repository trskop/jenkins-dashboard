{-# LANGUAGE NoImplicitPrelude #-}
module Utils.PlaySound
    ( playSound
    ) where

import Control.Monad (void)
import Data.Bool (Bool(False, True))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Nothing))
import Data.String (String)
import System.IO (FilePath, IO, IOMode(ReadWriteMode), withFile)
import System.Process
    ( CmdSpec(RawCommand)
    , CreateProcess
        ( CreateProcess
        , cmdspec
        , std_in
        , std_out
        , std_err
        , cwd
        , env
        , close_fds
        , create_group
        , delegate_ctlc
        )
    , StdStream(UseHandle)
    , createProcess
    )


player :: FilePath
player = "mplayer"

playerOpts :: FilePath -> [String]
playerOpts x = ["-really-quiet", x]

playSound :: String -> IO ()
playSound x = void . withFile "/dev/null" ReadWriteMode $ \ h ->
    createProcess CreateProcess
        { cmdspec = RawCommand player $ playerOpts x
        , std_in = UseHandle h
        , std_out = UseHandle h
        , std_err = UseHandle h
        , cwd = Nothing
        , env = Nothing
        , close_fds = True
        , create_group = False
        , delegate_ctlc = False
        }
