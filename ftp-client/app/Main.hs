module Main where

import Network.FTP.Client
import System.Environment
import Control.Monad

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    withFTP host port $ \h welcome -> do
        print welcome
        login h user pass
        print =<< nlst h []
