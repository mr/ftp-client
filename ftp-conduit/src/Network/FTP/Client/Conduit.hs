module Network.FTP.Client.Conduit (
    nlst
) where

import Data.Conduit.Binary
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Network.FTP.Client
    ( createTransferHandlePasv
    , sendCommand
    , sendCommands
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    )
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

getAllLineRespC :: MonadIO m => Handle -> Producer m ByteString
getAllLineRespC h = loop
    where
        loop = do
            eof <- liftIO $ hIsEOF h
            if eof
                then return ()
                else do
                    line <- liftIO $ getLineResp h
                    yield line
                    loop

nlst :: MonadIO m => Handle -> [String] -> Producer m ByteString
nlst h args = do
        th <- liftIO $ do
            th <- createTransferHandlePasv h
            sendCommands h [RType TA, Nlst args]
            return th
        getAllLineRespC th
