{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Applicative (Applicative((<*>)))
import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Monad (Monad((>>=)), forever)
import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Data.Bool (not)
import Data.Either (either)
import Data.Eq (Eq)
import Data.Function (const, flip, id, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, map, notElem, null, partition, union, unwords, (\\))
import Data.Monoid (mconcat, (<>))
import Pipes
import qualified Pipes.Prelude as P
import System.IO (FilePath, IO, putStrLn)

import Options.Applicative
    ( execParser
    , info
    , helper
    , fullDesc
    , progDesc
    )
import System.Console.ANSI
    ( Color(Green, Red, Yellow)
    , ColorIntensity(Dull)
    , ConsoleLayer(Foreground)
    , SGR(SetColor, Reset)
    , setSGRCode
    )

import Paths_jenkins_dashboard (getDataFileName)

import Jenkins.Get (getJenkinsJobs)
import Jenkins.Type
    ( Activity(Building, Idle)
    , Job
    , LastRun(Unstable, Failed, Success)
    , failedOrUnstable
    , getJobs
    , jobActivity
    , jobLastRun
    , name
    )
import Utils.PlaySound (playSound)
import Type
    ( Config(boringNames, credentials, refreshDelay, url)
    , M
    )
import Options (configOptions)

-- | Runs action every n nanoseconds.
everyNNanoseconds :: MonadIO m => Int -> m a -> Producer a m ()
everyNNanoseconds n a = forever $ do
    lift a >>= yield
    liftIO $ threadDelay n

-- | RSS-like functionality...
latest :: (Monad m, Eq y) => Pipe [y] [y] m a
latest = f [] where
    f s = do
        x <- await
        yield $ x \\ s
        f x

getter :: Producer [Job] M ()
getter = do
    d <- lift $ asks refreshDelay
    everyNNanoseconds d get
    where
    get = do
        c <- asks credentials
        u <- asks url
        liftIO (strip <$> getJenkinsJobs c u)
    strip = either (const []) getJobs

soundFailure :: FilePath
soundFailure = "sounds/failure.wav"

playOnNewError :: Consumer' [Job] M ()
playOnNewError = f [] >-> latest >-> P.filter (not . null) >-> g where
    f st = do
        s <- await
        let (addSt, remSt) = map name *** map name $ partition failedOrUnstable s
            st' = st `union` addSt \\ remSt
        yield st'
        f st'
    g = forever $ do
        _ <- await
        liftIO $ getDataFileName soundFailure >>= playSound

printJob :: Job -> Effect M ()
printJob = liftIO . putStrLn . format where
    format x = case jobActivity x of
        Idle -> (color <*> name) x
        Building -> building x
    color j = case jobLastRun j of
        Success -> addColor Green
        Failed -> addColor Red
        Unstable -> addColor Yellow
        _ -> id
    addColor c s = mconcat
        [ setSGRCode [SetColor Foreground Dull c]
        , s
        , setSGRCode [Reset]
        ]
    building x = unwords
        [ "Building"
        , name x
        ]

runWihtConfig :: Config -> IO ()
runWihtConfig c = flip runReaderT c . runEffect $ for magic printJob
    where
    magic = getter >-> P.map f >-> latest >-> P.tee playOnNewError >-> P.concat
    f = filter (flip notElem (boringNames c) . name)

main :: IO ()
main = execParser opts >>= runWihtConfig
    where
    opts = info (helper <*> configOptions)
        ( fullDesc
        <> progDesc "Monitor changes in Jenkins dashboard."
        )
