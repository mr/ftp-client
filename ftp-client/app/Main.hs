module Main where

import Network.FTP.Client
import System.Environment

main :: IO ()
main = do
    [host, sPort, user, pass, fileName] <- getArgs
    let port = read sPort
    withFTP host port $ \h welcome -> do
        print welcome
        print =<< login h user pass
        stor h fileName "dank\nmemes" TI
