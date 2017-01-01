module Network.FTP.Client.Conduit (
    nlst
) where

import Data.Conduit.Binary
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Network.FTP.Client
    ( createDataSocket
    , sendCommand
    , sendCommands
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    , ControlConnection(..)
    , ccClose
    , DataConnection(..)
    , dcClose
    , createSendDataCommand
    , PortActivity(..)
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

sourceDataCommand
    :: MonadResource m
    => ControlConnection
    -> PortActivity
    -> [FTPCommand]
    -> (DataConnection -> Producer m ByteString)
    -> Producer m ByteString
sourceDataCommand cc pa cmds = bracketP (createSendDataCommand cc pa cmds) dcClose

nlst :: MonadResource m => ControlConnection -> [String] -> Producer m ByteString
nlst cc args = sourceDataCommand cc Passive [RType TA, Nlst args] getAllLineRespC
