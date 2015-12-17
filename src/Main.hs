{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Applicative (Applicative((<*>)))
import Control.Concurrent (threadDelay)
import Control.Monad (Monad((>>=)), forever, when)
import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Data.Either (either)
import Data.Eq (Eq)
import Data.Function (($), (.), const, flip, id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List ((\\), any)
import Data.Monoid ((<>), mconcat)
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
    ( Color(Green, Red)
    , ColorIntensity(Dull)
    , ConsoleLayer(Foreground)
    , SGR(SetColor, Reset)
    , setSGRCode
    )

import Paths_jenkins_dashboard (getDataFileName)

import Jenkins.Get (getJenkinsJobs)
import Jenkins.Type
    ( LastRun(Failed, Success)
    , Job
    , getJobs
    , recentlyFailed
    , name
    , jobLastRun)
import Utils.PlaySound (playSound)
import Type
    ( Config(credentials, refreshDelay, url)
    , M
    )
import Options (configOptions)

-- | Runs action every n nanoseconds.
everyNNanoseconds :: MonadIO m => Int -> m a -> Producer a m ()
everyNNanoseconds n a = forever $ do
    lift a >>= yield
    liftIO . threadDelay $ n

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
        liftIO $ (strip <$> getJenkinsJobs c u)
    strip = either (const []) getJobs

soundFailure :: FilePath
soundFailure = "sounds/failure.wav"

playOnError :: Consumer' [Job] M ()
playOnError = forever $ do
    s <- await
    when (any recentlyFailed s) . liftIO
        $ getDataFileName soundFailure >>= playSound

printJob :: Job -> Effect M ()
printJob = liftIO . putStrLn . format where
    format = color <*> name
    color j = case jobLastRun j of
        Success -> addColor Green
        Failed -> addColor Red
        _ -> id
    addColor c s = mconcat
        [ setSGRCode [SetColor Foreground Dull c]
        , s
        , setSGRCode [Reset]
        ]

runWihtConfig :: Config -> IO ()
runWihtConfig c = flip runReaderT c . runEffect
        $ for magic printJob
        where
        magic = getter >-> latest >-> P.tee playOnError >-> P.concat

main :: IO ()
main = execParser opts >>= runWihtConfig
    where
    opts = info (helper <*> configOptions)
        ( fullDesc
        <> progDesc "Monitor changes in Jenkins dashboard."
        )
