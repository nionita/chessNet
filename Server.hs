module Server (
        main
    ) where

import Control.Concurrent
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as B8
import Network
import System.Directory (setCurrentDirectory)
import Commons

-- dummy
data Config = Config

-- TODO: we should count the number of engines running
-- and refuse new connections when reaching a define limit
-- Better to know about threads of each engine and take
-- this into account
main = do
    config <- readConfig
    withSocketsDo $ forever $ do
        socket <- listenOn chessNetPort
        (handle, client, _) <- accept socket
        when (isClientOk config client) $ void $ forkIO $ handleClient config handle

-- dummy
readConfig :: IO Config
readConfig = return Config

-- dummy
isClientOk :: Config -> String -> Bool
isClientOk _ _ = True

handleClient :: Config -> Handle -> IO ()
handleClient config h = do
    cdl <- B8.hGetLine h
    when (cd `B8.isPrefixOf` cdl) $ do
        let cwdir = B8.unpack $ B8.drop 3 cdl
        setCurrentDirectory cwdir
        stl <- B8.hGetLine h
        when (start `B8.isPrefixOf` stl) $ do
            let cmd8 = B8.drop 6 stl
                prg  = unpack . fst . B8.span (/= ' ') $ cmd8
            when (isProgramOk config prg)
               