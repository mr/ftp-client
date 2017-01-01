module Network.FTP.Client.Conduit (
    nlst,
    retr,
    stor
) where

import qualified Data.Conduit.Binary as CB
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
    , getMultiLineResp
    , sendLine
    )
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource
import Data.Monoid ((<>))

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

sendAllLineC :: MonadIO m => DataConnection -> Consumer ByteString m ()
sendAllLineC (DC h) = loop
    where
        loop = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just x -> do
                    liftIO $ sendLine h x
                    loop

sourceDataCommand
    :: MonadResource m
    => ControlConnection
    -> PortActivity
    -> [FTPCommand]
    -> (DataConnection -> ConduitM i o m r)
    -> ConduitM i o m r
sourceDataCommand cc@(CC ch) pa cmds f = do
    x <- bracketP (createSendDataCommand cc pa cmds) dcClose f
    resp <- liftIO $ getMultiLineResp ch
    liftIO $ print $ "Recieved: " <> (show resp)
    return x

nlst :: MonadResource m => ControlConnection -> [String] -> Producer m ByteString
nlst cc args = sourceDataCommand cc Passive [RType TA, Nlst args] getAllLineRespC

retr :: MonadResource m => ControlConnection -> String -> Producer m ByteString
retr cc path = sourceDataCommand cc Passive [RType TI, Retr path] (\(DC h) -> CB.sourceHandle h)

stor :: MonadResource m => ControlConnection -> String -> RTypeCode -> Consumer ByteString m ()
stor cc loc rtype = do
    sourceDataCommand cc Passive [RType rtype, Stor loc] $ \dc@(DC h) ->
        case rtype of
            TA -> sendAllLineC dc
            TI -> CB.sinkHandle h
