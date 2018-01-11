{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Applicative ((<*>))
import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Monad (Monad, (>>=), forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Control.Monad.Trans (lift)
import Data.Bool (not)
import Data.Either (either)
import Data.Eq (Eq)
import Data.Function (const, flip, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, map, notElem, null, partition, union, (\\))
import Data.Monoid (mconcat, (<>))
import System.IO (FilePath, IO, putStrLn)

import Options.Applicative
    ( execParser
    , info
    , helper
    , fullDesc
    , progDesc
    )
import Pipes
    ( Consumer'
    , Effect
    , Pipe
    , Producer
    , (>->)
    , await
    , for
    , runEffect
    , yield
    )
import qualified Pipes.Prelude as P
import System.Console.ANSI
    ( Color(Green, Red, White, Yellow)
    , ColorIntensity(Dull, Vivid)
    , ConsoleIntensity(BoldIntensity, FaintIntensity, NormalIntensity)
    , ConsoleLayer(Foreground)
    , SGR(SetColor, SetConsoleIntensity, Reset)
    , setSGRCode
    )

import Paths_jenkins_dashboard (getDataFileName)

import Jenkins.Get (getJenkinsJobs)
import Jenkins.Type
    ( Activity(Building, Idle)
    , Job
    , LastRun(Aborted, Disabled, Failed, NotBuilded, Success, Unstable)
    , failedOrUnstable
    , getJobs
    , jobActivity
    , jobLastRun
    , name
    )
import Utils.PlaySound (playSound)
import Type
    ( Config
        ( boringNames
        , refreshDelay
        , url
        , wreqOptions
        )
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
latest = f []
  where
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
        opts <- asks wreqOptions
        u <- asks url
        liftIO (strip <$> getJenkinsJobs opts u)
    strip = either (const []) getJobs

soundFailure :: FilePath
soundFailure = "sounds/failure.wav"

playOnNewError :: Consumer' [Job] M ()
playOnNewError = f [] >-> latest >-> P.filter (not . null) >-> g
  where
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
printJob = liftIO . putStrLn . format
  where
    format x = case jobActivity x of
        Idle -> (color <*> name) x
        Building -> addColor BoldIntensity Vivid White . building $ name x

    color j = case jobLastRun j of
        Success -> addColor NormalIntensity Dull Green . success
        Failed -> addColor NormalIntensity Dull Red . failure
        Unstable -> addColor NormalIntensity Dull Yellow . unstable
        Disabled -> addColor FaintIntensity Dull White . disabled
        Aborted -> addColor BoldIntensity Vivid White . aborted
        NotBuilded -> addColor FaintIntensity Dull White . notBuilt

    addColor i ci c s = mconcat
        [ setSGRCode [SetConsoleIntensity i, SetColor Foreground ci c]
        , s
        , setSGRCode [Reset]
        ]

    notBuilt s = mconcat ["NotBuilt  ", s]
    disabled s = mconcat ["Disabled  ", s]
    unstable s = mconcat ["Unstable  ", s]
    failure  s = mconcat ["Failure   ", s]
    success  s = mconcat ["Success   ", s]
    building s = mconcat ["Building  ", s]
    aborted  s = mconcat ["Aborted   ", s]

runWihtConfig :: Config -> IO ()
runWihtConfig c =
    flip runReaderT c . runEffect $ for magic printJob
  where
    magic =
        getter >-> P.map f >-> latest >-> P.tee playOnNewError >-> P.concat

    f = filter (flip notElem (boringNames c) . name)

main :: IO ()
main = execParser opts >>= runWihtConfig
  where
    opts = info (helper <*> configOptions)
        ( fullDesc
        <> progDesc "Monitor changes in Jenkins dashboard."
        )
