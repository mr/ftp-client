module Network.FTP.Client.Conduit (
    nlst,
    retr,
    stor
) where

import qualified Data.Conduit.Binary as CB
import Data.Conduit
import Control.Monad.IO.Class
import System.IO hiding (Handle)
import Network.FTP.Client
    ( createDataSocket
    , sendCommand
    , sendCommands
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    , createSendDataCommand
    , PortActivity(..)
    , getMultiLineResp
    , sIOHandleImpl
    )

import qualified Network.FTP.Client as FTP
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource
import Data.Monoid ((<>))

data Handle m = Handle
    { ftpHandle :: FTP.Handle
    , source :: Producer m ByteString
    , sink :: Consumer ByteString m ()
    }

getAllLineRespC :: MonadIO m => FTP.Handle -> Producer m ByteString
getAllLineRespC h = loop
    where
        loop = do
            eof <- liftIO $ isEOF h
            if eof
                then return ()
                else do
                    line <- liftIO $ FTP.recvLine h
                    yield line
                    loop

sendAllLineC :: MonadIO m => FTP.Handle -> Consumer ByteString m ()
sendAllLineC h = loop
    where
        loop = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just x -> do
                    liftIO $ FTP.sendLine h x
                    loop

sourceDataCommand
    :: MonadResource m
    => ControlConnection
    -> PortActivity
    -> [FTPCommand]
    -> (DataConnection -> ConduitM i o m r)
    -> ConduitM i o m r
sourceDataCommand ch pa cmds f = bracketP
    (createSendDataCommand cc pa cmds)
    SIO.hClose
    (\h -> do
        x <- f $ sIOHandleImpl h
        resp <- liftIO $ getMultiLineResp ch
        liftIO $ print $ "Recieved: " <> (show resp)
        return x
    )

nlst :: MonadResource m => ControlConnection -> [String] -> Producer m ByteString
nlst cc args = sourceDataCommand cc Passive [RType TA, Nlst args] getAllLineRespC

retr :: MonadResource m => ControlConnection -> String -> Producer m ByteString
retr cc path = sourceDataCommand cc Passive [RType TI, Retr path] source

stor :: MonadResource m => ControlConnection -> String -> RTypeCode -> Consumer ByteString m ()
stor cc loc rtype = do
    sourceDataCommand cc Passive [RType rtype, Stor loc] $ \dh ->
        case rtype of
            TA -> sendAllLineC dh
            TI -> sink dh
