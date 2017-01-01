module Main where

import Network.FTP.Client
import qualified Network.FTP.Client.Conduit as FC
import Conduit
import qualified Data.Conduit.Binary as CB
import System.Environment

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    withFTP host port $ \h welcome -> do
        print welcome
        print =<< login h user pass
        runConduitRes
            $ FC.retr h ".bash_profile"
            .| CB.sinkFile "test_bash_profile"
