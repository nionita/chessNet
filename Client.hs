module Client (
        main
    ) where

import Control.Applicative (<$>)
import Control.Exception.Base (bracket)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Lazy as M
import Network
import Common

confFile = "chessNetClient.txt"

data ClientConfig = ClientConfig M.Map String (String, String, String)

main = withSocketsDo $ do
    ClientConfig map <- readClientConfig
    me <- cleanName <$> getProgName
    case M.lookup me map of
        Just pair -> runClient pair
        Nothing   -> return ()

readClientConfig :: IO ClientConfig
readClientConfig = do
    cff <- B8.readFile confFile
    return $ M.fromList $ B8.lines cff $ filter nonComment $ map entry
    where nonComment l = not $ comm `B8.isPrefixOf` l
          comm = B8.pack "--"
          entry l = let (name, rest) = B8.break (== '=') l
                        (host, rest) = B8.break (== ':') rest
                        (dir,   cmd) = B8.break (== ':') rest
                    in (trim name, (trim host, trim dir, trim rest))

runClient :: (String, String, String) -> IO ()
runClient (host, dir, cmd) = bracket (connectTo host chessNetPort)
                                     (\h -> B8.hPutStrLn h exit >> hClose h)
                                     startClient dir cmd

startClient :: String -> String -> Handle -> IO ()
startClient dir cmd h = do
    doCd h dir
    doStart h cmd
    ioToNet False