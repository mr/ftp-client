{-|
Module      : Network.FTP.Client
Description : Transfer files over FTP and FTPS with Conduit
License     : Public Domain
Stability   : experimental
Portability : POSIX
-}
module Network.FTP.Client.Conduit (
    -- * Data commands
    nlst,
    retr,
    list,
    stor,
    -- * Secure data commands
    nlstS,
    retrS,
    listS,
    storS
) where

import Data.Conduit
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO
import Network.FTP.Client
    ( sendCommand
    , sendCommands
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    , createSendDataCommand
    , createTLSSendDataCommand
    , PortActivity(..)
    , getMultiLineResp
    , sIOHandleImpl
    , tlsHandleImpl
    )

import qualified Network.FTP.Client as FTP
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource
import Data.Monoid ((<>))
import System.IO.Error
import Network.Connection

getAllLineRespC :: MonadIO m => FTP.Handle -> Producer m ByteString
getAllLineRespC h = loop
    where
        loop = do
            line <- liftIO $ FTP.getLineResp h
                `catchIOError` (\_ -> return "")
            if B.null line
                then return ()
                else do
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
    => FTP.Handle
    -> PortActivity
    -> [FTPCommand]
    -> (FTP.Handle -> ConduitM i o m r)
    -> ConduitM i o m r
sourceDataCommand ch pa cmds f = do
    x <- bracketP
        (createSendDataCommand ch pa cmds)
        hClose
        (f . sIOHandleImpl)
    resp <- liftIO $ getMultiLineResp ch
    liftIO $ print $ "Recieved: " <> (show resp)
    return x

sourceTLSDataCommand
    :: MonadResource m
    => FTP.Handle
    -> PortActivity
    -> [FTPCommand]
    -> (FTP.Handle -> ConduitM i o m r)
    -> ConduitM i o m r
sourceTLSDataCommand ch pa cmds f = do
    x <- bracketP
        (createTLSSendDataCommand ch pa cmds)
        connectionClose
        (f . tlsHandleImpl)
    resp <- liftIO $ getMultiLineResp ch
    liftIO $ print $ "Recieved: " <> (show resp)
    return x

sourceHandle :: MonadIO m => FTP.Handle -> Producer m ByteString
sourceHandle h = loop
    where
        loop = do
            bs <- liftIO $ FTP.recv h defaultChunkSize
                `catchIOError` (\_ -> return "")
            if B.null bs
                then return ()
                else do
                    yield bs
                    loop

sinkHandle :: MonadIO m => FTP.Handle -> Consumer ByteString m ()
sinkHandle h = loop
    where
        loop = do
            mbs <- await
            case mbs of
                Nothing -> return ()
                Just bs -> do
                    liftIO $ FTP.send h bs
                    loop

sendType :: MonadResource m => RTypeCode -> FTP.Handle -> Consumer ByteString m ()
sendType TA h = sendAllLineC h
sendType TI h = sinkHandle h

nlst :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
nlst ch args = sourceDataCommand ch Passive [RType TA, Nlst args] getAllLineRespC

retr :: MonadResource m => FTP.Handle -> String -> Producer m ByteString
retr ch path = sourceDataCommand ch Passive [RType TI, Retr path] sourceHandle

list :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
list ch args = sourceDataCommand ch Passive [RType TA, List args] getAllLineRespC

stor :: MonadResource m => FTP.Handle -> String -> RTypeCode -> Consumer ByteString m ()
stor ch loc rtype =
    sourceDataCommand ch Passive [RType rtype, Stor loc] $ sendType rtype

-- TLS

nlstS :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
nlstS ch args = sourceTLSDataCommand ch Passive [RType TA, Nlst args] getAllLineRespC

retrS :: MonadResource m => FTP.Handle -> String -> Producer m ByteString
retrS ch path = sourceTLSDataCommand ch Passive [RType TI, Retr path] sourceHandle

listS :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
listS ch args = sourceTLSDataCommand ch Passive [RType TA, List args] getAllLineRespC

storS :: MonadResource m => FTP.Handle -> String -> RTypeCode -> Consumer ByteString m ()
storS ch loc rtype =
    sourceTLSDataCommand ch Passive [RType rtype, Stor loc] $ sendType rtype
