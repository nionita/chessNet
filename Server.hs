module Main (
        main
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B8
import Data.List
import Network
import System.Directory
import System.IO
import System.Process

import Common

-- dummy
data Config = Config

type CtxIO a = ReaderT Config IO a

-- TODO: we should count the number of engines running
-- and refuse new connections when reaching a define limit
-- Better to know about threads of each engine and take
-- this into account
main = do
    config <- readConfig
    withSocketsDo $ forever $ do
        socket <- listenOn chessNetPort
        (handle, client, _) <- accept socket
        forkFinally (
                checkClient config client $
                    runReaderT (handleClient handle) Config
            )
            (\_ -> hClose handle)

-- dummy
readConfig :: IO Config
readConfig = return Config

-- TODO: check client permission, log the request etc
checkClient :: Config -> String -> IO () -> IO ()
checkClient _ client act = do
    putStrLn $ "Client " ++ client ++ " wants to connect"
    if "192.168.1." `isPrefixOf` client
       then do
           putStrLn "Client accepted"
           act
       else putStrLn "Client refused"

-- dummy
-- TODO: check if program is allowed to be start, log etc
checkProg :: String -> CtxIO () -> CtxIO ()
checkProg _ act = act

-- TODO: write log when not what expected
expectCmd :: Handle -> B8.ByteString -> (B8.ByteString -> CtxIO ()) -> CtxIO ()
expectCmd h bcmd act = do
    bline <- liftIO $ B8.hGetLine h
    if bcmd `B8.isPrefixOf` bline
       then do	-- got what expected: take line rest and run the action with it
           let rest = B8.drop (B8.length bcmd) bline
           act rest
       else return ()	-- did not get what expected: should write log and exit

handleClient :: Handle -> CtxIO ()
handleClient hnet = do
    liftIO $ hSetBuffering hnet LineBuffering
    expectCmd hnet cd $ \cdrest -> do
        let cwdir = B8.unpack cdrest
        expectCmd hnet start $ \strest -> do
            case map B8.unpack $ B8.words strest of
                []         -> error "After start: what?"	-- TODO: make proper with logging
                (prg:args) -> checkProg prg $ liftIO
                                  $ bracket (openFile "stdError.txt" WriteMode) hClose	-- file name?
                                  $ \herl -> do
                                      (hinp, hout, herr, hprc) <- runInteractiveProcess prg args (Just cwdir) Nothing
                                      liftIO $ do
                                          ch  <- newChan
                                          tnp <- forkThenSignal ch (hnet `fromHdlToHdl` hinp)
                                          tpn <- forkThenSignal ch (hout `fromHdlToHdl` hnet)
                                          ter <- forkThenSignal ch (herr `fromHdlToHdl` herl)
                                          _ <- readChan ch
                                          -- one will be already done, so error - what is then?
                                          killThread tnp
                                          killThread tnp
                                          killThread ter
