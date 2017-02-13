module Main where

import Network.FTP.Client

main :: IO ()
main = withFTPS "hostname.com" 21 $ \h welcome -> do
    login h "username" "password"
    print =<< mlsd h "."
