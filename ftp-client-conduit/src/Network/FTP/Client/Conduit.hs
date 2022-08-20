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
    mlsd
) where

import Conduit
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO
import Network.FTP.Client
    ( sendCommand
    , sendCommandS
    , sendAll
    , sendAllS
    , FTPCommand(..)
    , RTypeCode(..)
    , getLineResp
    , createSendDataCommand
    , createTLSSendDataCommand
    , PortActivity(..)
    , getResponse
    , getResponseS
    , sIOHandleImpl
    , tlsHandleImpl
    , Security(..)
    , parseMlsxLine
    )

import qualified Network.FTP.Client as FTP
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Monad.Trans.Resource
import Data.Monoid ((<>))
import System.IO.Error
import Network.Connection
import qualified Control.Monad.Catch as M

debugging :: Bool
debugging = False

debugPrint :: (Show a, MonadIO m) => a -> m ()
debugPrint s = debugPrint' s debugging
    where
        debugPrint' _ False = return ()
        debugPrint' s True = liftIO $ print s

debugResponse :: (Show a, MonadIO m) => a -> m ()
debugResponse s = debugPrint $ "Recieved: " <> (show s)

getAllLineRespC :: MonadIO m => FTP.Handle -> ConduitT i ByteString m ()
getAllLineRespC h = loop
    where
        loop = do
            line <- liftIO
                $ FTP.getLineResp h `M.catchIOError` const (return "")
            if B.null line
                then return ()
                else do
                    yield line
                    loop

sendAllLineC :: MonadIO m => FTP.Handle -> ConduitT ByteString o m ()
sendAllLineC h = loop
    where
        loop = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just x -> do
                    liftIO $ FTP.sendLine h x
                    loop

sourceDataCommandSecurity
    :: MonadResource m
    => FTP.Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (FTP.Handle -> ConduitM i o m r)
    -> ConduitM i o m r
sourceDataCommandSecurity h =
    case FTP.security h of
        Clear -> sourceDataCommand h
        TLS -> sourceTLSDataCommand h

sourceDataCommand
    :: MonadResource m
    => FTP.Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (FTP.Handle -> ConduitM i o m r)
    -> ConduitM i o m r
sourceDataCommand ch pa code cmd f = do
    sendCommandS ch $ RType code
    x <- bracketP
        (createSendDataCommand ch pa cmd)
        (liftIO . hClose)
        (f . sIOHandleImpl)
    resp <- getResponse ch
    debugResponse resp
    return x

sourceTLSDataCommand
    :: MonadResource m
    => FTP.Handle
    -> PortActivity
    -> RTypeCode
    -> FTPCommand
    -> (FTP.Handle -> ConduitM i o m r)
    -> ConduitM i o m r
sourceTLSDataCommand ch pa code cmd f = do
    sendCommandS ch $ RType code
    x <- bracketP
        (createTLSSendDataCommand ch pa cmd)
        (liftIO . connectionClose)
        (f . tlsHandleImpl)
    resp <- getResponse ch
    debugResponse resp
    return x

sourceFTPHandle :: MonadIO m => FTP.Handle -> ConduitT i ByteString m ()
sourceFTPHandle h = loop
    where
        loop = do
            bs <- liftIO $ FTP.recv h defaultChunkSize
                `M.catchIOError` const (return "")
            if B.null bs
                then return ()
                else do
                    yield bs
                    loop

sinkFTPHandle :: MonadIO m => FTP.Handle -> ConduitT ByteString o m ()
sinkFTPHandle h = loop
    where
        loop = do
            mbs <- await
            case mbs of
                Nothing -> return ()
                Just bs -> do
                    liftIO $ FTP.send h bs
                    loop

sendType
    :: MonadResource m
    => RTypeCode
    -> FTP.Handle
    -> ConduitT ByteString o m ()
sendType TA h = sendAllLineC h
sendType TI h = sinkFTPHandle h

nlst :: MonadResource m => FTP.Handle -> [String] -> ConduitT i ByteString m ()
nlst ch args =
    sourceDataCommandSecurity ch Passive TA (Nlst args) getAllLineRespC

retr :: MonadResource m => FTP.Handle -> String -> ConduitT i ByteString m ()
retr ch path =
    sourceDataCommandSecurity ch Passive TI (Retr path) sourceFTPHandle

list :: MonadResource m => FTP.Handle -> [String] -> ConduitT i ByteString m ()
list ch args =
    sourceDataCommandSecurity ch Passive TA (List args) getAllLineRespC

stor
    :: MonadResource m
    => FTP.Handle
    -> String
    -> RTypeCode
    -> ConduitT ByteString o m ()
stor ch loc rtype =
    sourceDataCommandSecurity ch Passive rtype (Stor loc) $ sendType rtype

mlsd
    :: MonadResource m
    => FTP.Handle
    -> String
    -> ConduitT i FTP.MlsxResponse m ()
mlsd ch dir =
    sourceDataCommandSecurity ch Passive TA (Mlsd dir) getAllLineRespC
        .| mapC parseMlsxLine
