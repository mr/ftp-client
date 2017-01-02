module Main where

import Network.FTP.Client
import System.Environment
import Control.Monad

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    withFTP host port $ \cc welcome -> do
        print welcome
        login cc user pass
        cwd cc "files"
        void $ pwd cc
