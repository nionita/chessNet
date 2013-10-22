module Common (
        chessNetPort, exit, cd, start,
        doCd, doStart,
        trim,
        forkThenSignal,
        fromHdlToHdl
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B8
import Network
import System.IO

chessNetPort = PortNumber 14000

exit  = B8.pack "EXIT"
cd    = B8.pack "CD "
start = B8.pack "START "

doCd h x    = B8.hPutStrLn h $ B8.append cd x
doStart h x = B8.hPutStrLn h $ B8.append start x

trim = B8.dropWhile (== ' ') . fst . B8.spanEnd (== ' ')

-- Forks a thread with the given action and after it ends, signals this on the given channel
forkThenSignal :: Chan () -> IO () -> IO ThreadId
forkThenSignal ch act = forkFinally act $ \_ -> writeChan ch ()

-- Copy from one handle to another in an endless loop
fromHdlToHdl :: Handle -> Handle -> IO ()
fromHdlToHdl hi ho = forever $ do
    line <- B8.hGetLine hi
    B8.hPutStrLn ho line
