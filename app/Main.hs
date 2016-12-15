module Main where

import Network.FTP.Client
import System.Environment

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    testFTP host port user pass
