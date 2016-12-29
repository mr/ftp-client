module Main where

import Network.FTP.Client
import qualified Network.FTP.Client.Conduit as FC
import Conduit
import System.Environment

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    withFTP host port $ \h welcome -> do
        print welcome
        print =<< login h user pass
        runConduit $ FC.nlst h [] .| mapM_C print
