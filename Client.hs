module Main (
        main
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception.Base (bracket)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Network
import System.Environment (getProgName)
import System.IO
import Common

confFile = "chessNetClient.txt"

data ClientConfig = ClientConfig (M.Map String (String, String, String))

main = withSocketsDo $ do
    ClientConfig map <- readClientConfig
    me <- cleanName <$> getProgName
    case M.lookup me map of
        Just pair -> runClient pair
        Nothing   -> return ()

cleanName :: String -> String
cleanName = id	-- must see what must be done

readClientConfig :: IO ClientConfig
readClientConfig = do
    cff <- B8.readFile confFile
    return $ ClientConfig $ M.fromList $ map entry $ filter nonComment $ B8.lines cff
    where nonComment l = not $ comm `B8.isPrefixOf` l
          comm = B8.pack "--"
          entry l = let (name, rest1) = B8.break (== '=') l
                        (host, rest2) = B8.break (== ':') rest1
                        (dir,    cmd) = B8.break (== ':') rest2
                    in (B8.unpack $ trim name,
                       (B8.unpack $ trim host, B8.unpack $ trim dir, B8.unpack $ trim cmd))

runClient :: (String, String, String) -> IO ()
runClient (host, dir, cmd) = bracket (connectTo host chessNetPort)
                                     (\h -> B8.hPutStrLn h exit >> hClose h)
                                     (startClient dir cmd)

startClient :: String -> String -> Handle -> IO ()
startClient dir cmd h = do
    hSetBuffering h LineBuffering
    doCd h $ B8.pack dir
    doStart h $ B8.pack cmd
    ch  <- newChan
    tpn <- forkThenSignal ch (stdin `fromHdlToHdl` h)
    tnp <- forkThenSignal ch (h     `fromHdlToHdl` stdout)
    _ <- readChan ch
    killThread tpn	-- one of them will error out (already terminated) - what then?
    killThread tnp