module Main where

import Network.FTP.Client
import qualified Network.FTP.Client.Conduit as FC
import Conduit
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Monoid ((<>))

main :: IO ()
main = withFTPS "hostname.com" 21 $ \h welcome -> do
    login h "username" "password"
    runConduitRes
        $ FC.mlsd h "."
        .| mapC (C.pack . (<> "\n") . show)
        .| stdoutC
