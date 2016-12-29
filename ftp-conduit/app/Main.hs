module Main where

import Network.FTP.Client
import qualified Network.FTP.Conduit as FC
import Data.Conduit
import System.Environment

main :: IO ()
main = do
    [host, sPort, user, pass] <- getArgs
    let port = read sPort
    withFTP host port $ \h welcome -> do
        print welcome
        print =<< login h user pass
        print =<< runConduit (FC.nlst h [])
