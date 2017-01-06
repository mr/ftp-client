module Network.FTP.Client.Conduit (
    nlst,
    retr,
    stor
) where

import qualified Data.Conduit.Binary as CB
import Data.Conduit
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO
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
import System.IO.Error

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

nlst :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
nlst ch args = sourceDataCommand ch Passive [RType TA, Nlst args] getAllLineRespC

retr :: MonadResource m => FTP.Handle -> String -> Producer m ByteString
retr ch path = sourceDataCommand ch Passive [RType TI, Retr path] sourceHandle

list :: MonadResource m => FTP.Handle -> [String] -> Producer m ByteString
list ch args = sourceDataCommand ch Passive [RType TA, List args] getAllLineRespC

stor :: MonadResource m => FTP.Handle -> String -> RTypeCode -> Consumer ByteString m ()
stor ch loc rtype = do
    sourceDataCommand ch Passive [RType rtype, Stor loc] $ \dh ->
        case rtype of
            TA -> sendAllLineC dh
            TI -> sinkHandle dh
