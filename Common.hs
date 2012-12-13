module Common (
        chessPort, exit, cd,
        doCd, doStart,
        trim,
        ioToNet
    ) where

import qualified Data.ByteString.Char8 as B8
import Network

chessNetPort = 14000

exit  = B8.pack "EXIT"
cd    = B8.pack "CD "
start = B8.pack "START "

doCd h x    = B8.hPutStrLn $ B8.append cd x
doStart h x = B8.hPutStrLn $ B8.append start x

trim = B8.dropWhile (== ' ') . fst . B8.spanEnd B8.isSpace