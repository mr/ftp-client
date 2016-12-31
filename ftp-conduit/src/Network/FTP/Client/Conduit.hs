module Network.FTP.Client.Conduit (
    nlst
) where

import Data.Conduit.Binary
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Network.FTP.Client
    ( createDataConnectionPasv
    , sendCommand
    , sendCommands
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    , ControlConnection(..)
    , ccClose
    , DataConnection(..)
    , dcClose
    )
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource

getAllLineRespC :: MonadIO m => DataConnection -> Producer m ByteString
getAllLineRespC (DC h) = loop
    where
        loop = do
            eof <- liftIO $ hIsEOF h
            if eof
                then return ()
                else do
                    line <- liftIO $ getLineResp h
                    yield line
                    loop

nlst :: MonadResource m => ControlConnection -> [String] -> Producer m ByteString
nlst cc args = bracketP
    (do
        dc <- createDataConnectionPasv cc
        sendCommands cc [RType TA, Nlst args]
        return dc
    )
    dcClose
    getAllLineRespC
