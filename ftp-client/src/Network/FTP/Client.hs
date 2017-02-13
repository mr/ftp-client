{-|
Module      : Network.FTP.Client
Description : Transfer files over FTP and FTPS
License     : Public Domain
Stability   : experimental
Portability : POSIX
-}
module Network.FTP.Client (
    -- * Main Entrypoints
    withFTP,
    withFTPS,
    -- * Control Commands
    login,
    pasv,
    rename,
    dele,
    cwd,
    size,
    mkd,
    rmd,
    pwd,
    quit,
    -- * Data Commands
    nlst,
    retr,
    list,
    stor,
    mlsd,
    -- * Types
    FTPCommand(..),
    FTPResponse(..),
    ResponseStatus(..),
    MlsdResponse(..),
    RTypeCode(..),
    PortActivity(..),
    ProtType(..),
    Security(..),
    Handle(..),
    -- * Handle Implementations
    sIOHandleImpl,
    tlsHandleImpl,
    -- * Lower Level Functions
    sendCommand,
    sendCommands,
    getLineResp,
    getMultiLineResp,
    sendCommandLine,
    createSendDataCommand,
    createTLSSendDataCommand,
    parseMlsdLine
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.List
import Data.Attoparsec.ByteString.Char8
import qualified Network.Socket as S
import qualified System.IO as SIO
import Data.Monoid ((<>), mconcat)
import Control.Exception
import Control.Monad.Catch (MonadCatch, MonadMask)
import qualified Control.Monad.Catch as M
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Network.Connection
import System.IO.Error
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow

import Debug.Trace

debugging :: Bool
debugging = False

debugPrint :: (Show a, MonadIO m) => a -> m ()
debugPrint s = debugPrint' s debugging
    where
        debugPrint' _ False = return ()
        debugPrint' s True = liftIO $ print s

data Security = Clear | TLS

-- | Can send and recieve a 'Data.ByteString.ByteString'.
data Handle = Handle
    { send :: ByteString -> IO ()
    , sendLine :: ByteString -> IO ()
    , recv :: Int -> IO ByteString
    , recvLine :: IO ByteString
    , security :: Security
    }

-- | Response from an FTP command. ex "200 Welcome!"
data FTPResponse = FTPResponse {
    frStatus :: ResponseStatus, -- ^ Interpretation of the first digit of an FTP response code
    frCode :: Int, -- ^ The three digit response code
    frMessage :: ByteString -- ^ Text of the response
}

instance Show FTPResponse where
    show fr = (show $ frCode fr) <> " " <> (C.unpack $ frMessage fr)

-- | First digit of an FTP response
data ResponseStatus
    = Wait -- ^ 1
    | Success -- ^ 2
    | Continue -- ^ 3
    | FailureRetry -- ^ 4
    | Failure -- ^ 5
    deriving (Show)

responseStatus :: ByteString -> ResponseStatus
responseStatus cbs =
    case C.uncons cbs of
        Just ('1', _) -> Wait
        Just ('2', _) -> Success
        Just ('3', _) -> Continue
        Just ('4', _) -> FailureRetry
        _             -> Failure

data RTypeCode = TA | TI

serialzeRTypeCode :: RTypeCode -> String
serialzeRTypeCode TA = "A"
serialzeRTypeCode TI = "I"

data PortActivity = Active | Passive

data ProtType = P | C

-- | Commands according to the FTP specification
data FTPCommand
    = User String
    | Pass String
    | Acct String
    | RType RTypeCode
    | Retr String
    | Nlst [String]
    | Port S.HostAddress S.PortNumber
    | Stor String
    | List [String]
    | Rnfr String
    | Rnto String
    | Dele String
    | Size String
    | Mkd String
    | Rmd String
    | Pbsz Int
    | Prot ProtType
    | Mlsd String
    | Cwd String
    | Cdup
    | Ccc
    | Auth
    | Pwd
    | Abor
    | Pasv
    | Quit

instance Show FTPCommand where
    show = serializeCommand

formatPort :: S.HostAddress -> S.PortNumber -> String
formatPort ha pn =
    let (w1, w2, w3, w4) = S.hostAddressToTuple ha
        hn = show <$> [w1, w2, w3, w4]
        portParts = show <$> [pn `quot` 256, pn `mod` 256]
    in  intercalate "," (hn <> portParts)

serializeCommand :: FTPCommand -> String
serializeCommand (User user)  = "USER " <> user
serializeCommand (Pass pass)  = "PASS " <> pass
serializeCommand (Acct acct)  = "ACCT " <> acct
serializeCommand (RType rt)   = "TYPE " <> serialzeRTypeCode rt
serializeCommand (Retr file)  = "RETR " <> file
serializeCommand (Nlst [])    = "NLST"
serializeCommand (Nlst args)  = "NLST " <> intercalate " " args
serializeCommand (Port ha pn) = "PORT " <> formatPort ha pn
serializeCommand (Stor loc)   = "STOR " <> loc
serializeCommand (List [])    = "LIST"
serializeCommand (List args)  = "LIST " <> intercalate " " args
serializeCommand (Rnfr from)  = "RNFR " <> from
serializeCommand (Rnto to)    = "RNTO " <> to
serializeCommand (Dele file)  = "DELE " <> file
serializeCommand (Size file)  = "SIZE " <> file
serializeCommand (Mkd dir)    = "MKD " <> dir
serializeCommand (Rmd dir)    = "RMD " <> dir
serializeCommand (Pbsz buf)   = "PBSZ " <> show buf
serializeCommand (Prot P)     = "PROT P"
serializeCommand (Prot C)     = "PROT C"
serializeCommand (Mlsd path)  = "MLSD " <> path
serializeCommand (Cwd dir)    = "CWD " <> dir
serializeCommand Cdup         = "CDUP"
serializeCommand Ccc          = "CCC"
serializeCommand Auth         = "AUTH TLS"
serializeCommand Pwd          = "PWD"
serializeCommand Abor         = "ABOR"
serializeCommand Pasv         = "PASV"
serializeCommand Quit         = "QUIT"

stripCLRF :: ByteString -> ByteString
stripCLRF = C.takeWhile $ (&&) <$> (/= '\r') <*> (/= '\n')

-- | Get a line from the server
getLineResp :: Handle -> IO ByteString
getLineResp h = stripCLRF <$> recvLine h

-- | Get a full response from the server
-- Used in 'sendCommand'
getMultiLineResp :: MonadIO m => Handle -> m FTPResponse
getMultiLineResp h = do
    line <- liftIO $ getLineResp h
    let (code, rest) = C.splitAt 3 line
    message <- if C.head rest == '-'
        then loopMultiLine h code line
        else return line
    return $ FTPResponse
        (responseStatus code)
        (read $ C.unpack code)
        (C.drop 4 message)

loopMultiLine
    :: MonadIO m
    => Handle
    -> ByteString
    -> ByteString
    -> m ByteString
loopMultiLine h code line = do
    nextLine <- liftIO $ getLineResp h
    let multiLine = line <> "\n" <> nextLine
        nextCode = C.take 3 nextLine
    if nextCode == code
        then return multiLine
        else loopMultiLine h nextCode multiLine

sendCommandLine :: MonadIO m => Handle -> ByteString -> m ()
sendCommandLine h = liftIO . send h . (<> "\r\n")

-- | Send a command to the server and get a response back.
-- Some commands use a data 'Handle', and their data is not returned here.
sendCommand :: MonadIO m => Handle -> FTPCommand -> m FTPResponse
sendCommand h fc = do
    let command = serializeCommand fc
    debugPrint $ "Sending: " <> command
    sendCommandLine h $ C.pack command
    resp <- getMultiLineResp h
    debugPrint $ "Recieved: " <> (show resp)
    return resp

-- | Equvalent to
--
-- > mapM . sendCommand
sendCommands :: MonadIO m => Handle -> [FTPCommand] -> m [FTPResponse]
sendCommands = mapM . sendCommand

-- Control connection

createSocket :: MonadIO m => Maybe String -> Int -> S.AddrInfo -> m (S.Socket, S.AddrInfo)
createSocket host portNum hints = do
    addr:_ <- liftIO $ S.getAddrInfo (Just hints) host (Just $ show portNum)
    debugPrint $ "Addr: " <> show addr
    sock <- liftIO $ S.socket
        (S.addrFamily addr)
        (S.addrSocketType addr)
        (S.addrProtocol addr)
    return (sock, addr)

withSocketPassive
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (S.Socket -> m a)
    -> m a
withSocketPassive host portNum f = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream
    }
    M.bracketOnError
        (createSocket (Just host) portNum hints)
        (liftIO . S.close . fst)
        (\(sock, addr) -> do
            liftIO $ S.connect sock (S.addrAddress addr)
            debugPrint "Connected"
            f sock
        )

withSocketActive :: (MonadIO m, MonadMask m) => (S.Socket -> m a) -> m a
withSocketActive f = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream,
        S.addrFlags = [S.AI_PASSIVE]
    }
    M.bracketOnError
        (createSocket Nothing 0 hints)
        (liftIO . S.close . fst)
        (\(sock, addr) -> do
            liftIO $ S.bind sock (S.addrAddress addr)
            liftIO $ S.listen sock 1
            debugPrint "Listening"
            f sock
        )

createSIOHandle :: (MonadIO m, MonadMask m) => String -> Int -> m SIO.Handle
createSIOHandle host portNum = withSocketPassive host portNum
    $ liftIO . flip S.socketToHandle SIO.ReadWriteMode

sIOHandleImpl :: SIO.Handle -> Handle
sIOHandleImpl h = Handle
    { send = C.hPut h
    , sendLine = C.hPutStrLn h
    , recv = C.hGetSome h
    , recvLine = C.hGetLine h
    , security = Clear
    }

withSIOHandle
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (Handle -> m a)
    -> m a
withSIOHandle host portNum f = M.bracket
    (liftIO $ createSIOHandle host portNum)
    (liftIO . SIO.hClose)
    (f . sIOHandleImpl)

-- | Takes a host name and port. A handle for interacting with the server
-- will be returned in a callback.
--
-- @
-- withFTP "ftp.server.com" 21 $ \h welcome -> do
--     print welcome
--     login h "username" "password"
--     print =<< nlst h []
-- @
withFTP
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withFTP host portNum f = withSIOHandle host portNum $ \h -> do
    resp <- getMultiLineResp h
    f h resp

-- Data connection

withDataSocketPasv
    :: (MonadIO m, MonadMask m)
    => Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocketPasv h f = do
    (host, portNum) <- pasv h
    debugPrint $ "Host: " <> host
    debugPrint $ "Port: " <> show portNum
    withSocketPassive host portNum f

withDataSocketActive
    :: (MonadIO m, MonadMask m)
    => Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocketActive h f = withSocketActive $ \socket -> do
    (S.SockAddrInet sPort sHost) <- liftIO $ S.getSocketName socket
    port h sHost sPort
    f socket

-- | Open a socket that can be used for data transfers
withDataSocket
    :: (MonadIO m, MonadMask m)
    => PortActivity
    -> Handle
    -> (S.Socket -> m a)
    -> m a
withDataSocket Active  = withDataSocketActive
withDataSocket Passive = withDataSocketPasv

acceptData :: MonadIO m => PortActivity -> S.Socket -> m S.Socket
acceptData Passive = return
acceptData Active = return . fst <=< liftIO . S.accept

-- | Send setup commands to the server and
-- create a data 'System.IO.Handle'
createSendDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> m (SIO.Handle)
createSendDataCommand h pa cmds = withDataSocket pa h $ \socket -> do
    sendCommands h cmds
    acceptedSock <- acceptData pa socket
    liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode

-- | Provides a data 'Handle' in a callback for a command
withDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> (Handle -> m a)
    -> m a
withDataCommand ch pa cmds f = do
    x <- M.bracket
        (createSendDataCommand ch pa cmds)
        (liftIO . SIO.hClose)
        (f . sIOHandleImpl)
    resp <- getMultiLineResp ch
    debugPrint $ "Recieved: " <> (show resp)
    return x

-- | Recieve data and interpret it linewise
getAllLineResp :: (MonadIO m, MonadCatch m) => Handle -> m ByteString
getAllLineResp h = getAllLineResp' h []
    where
        getAllLineResp' h ret = ( do
            line <- liftIO $ getLineResp h
            getAllLineResp' h (ret <> [line]))
                `M.catchIOError` (\_ -> return $ C.intercalate "\n" ret)

-- | Recieve all data and return it as a 'Data.ByteString.ByteString'
recvAll :: (MonadIO m, MonadCatch m) => Handle -> m ByteString
recvAll h = recvAll' ""
    where
        recvAll' bs = ( do
            chunk <- liftIO $ recv h defaultChunkSize
            recvAll' $ bs <> chunk)
                `M.catchIOError` (\_ -> return bs)

-- TLS connection

connectTLS :: MonadIO m => SIO.Handle -> String -> Int -> m Connection
connectTLS h host portNum = do
    context <- liftIO initConnectionContext
    let tlsSettings = TLSSettingsSimple
            { settingDisableCertificateValidation = True
            , settingDisableSession = False
            , settingUseServerName = False
            }
        connectionParams = ConnectionParams
            { connectionHostname = host
            , connectionPort = toEnum . fromEnum $ portNum
            , connectionUseSecure = Just tlsSettings
            , connectionUseSocks = Nothing
            }
    liftIO $ connectFromHandle context h connectionParams

createTLSConnection
    :: (MonadIO m, MonadMask m)
    => String
    -> Int
    -> m (FTPResponse, Connection)
createTLSConnection host portNum = do
    h <- createSIOHandle host portNum
    let insecureH = sIOHandleImpl h
    resp <- getMultiLineResp insecureH
    sendCommand insecureH Auth
    conn <- connectTLS h host portNum
    return (resp, conn)

tlsHandleImpl :: Connection -> Handle
tlsHandleImpl c = Handle
    { send = connectionPut c
    , sendLine = connectionPut c . (<> "\n")
    , recv = connectionGet c
    , recvLine = connectionGetLine maxBound c
    , security = TLS
    }

withTLSHandle
    :: (MonadMask m, MonadIO m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withTLSHandle host portNum f = M.bracket
    (createTLSConnection host portNum)
    (liftIO . connectionClose . snd)
    (\(resp, conn) -> f (tlsHandleImpl conn) resp)

-- | Takes a host name and port. A handle for interacting with the server
-- will be returned in a callback. The commands will be protected with TLS.
--
-- @
-- withFTPS "ftps.server.com" 21 $ \h welcome -> do
--     print welcome
--     login h "username" "password"
--     print =<< nlst h []
-- @
withFTPS
    :: (MonadMask m, MonadIO m)
    => String
    -> Int
    -> (Handle -> FTPResponse -> m a)
    -> m a
withFTPS host portNum = withTLSHandle host portNum

-- TLS data connection

-- | Send setup commands to the server and
-- create a data TLS connection
createTLSSendDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> m Connection
createTLSSendDataCommand ch pa cmds = do
    sendCommands ch [Pbsz 0, Prot P]
    withDataSocket pa ch $ \socket -> do
        sendCommands ch cmds
        acceptedSock <- acceptData pa socket
        (S.SockAddrInet sPort sHost) <- liftIO $ S.getSocketName acceptedSock
        let (h1, h2, h3, h4) = S.hostAddressToTuple sHost
            hostName = intercalate "." $ (show . fromEnum) <$> [h1, h2, h3, h4]
        h <- liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode
        liftIO $ connectTLS h hostName (fromEnum sPort)

withTLSDataCommand
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> (Handle -> m a)
    -> m a
withTLSDataCommand ch pa cmds f = do
    x <- M.bracket
        (createTLSSendDataCommand ch pa cmds)
        (liftIO . connectionClose)
        (f . tlsHandleImpl)
    resp <- getMultiLineResp ch
    debugPrint $ "Recieved: " <> (show resp)
    return x

parse227 :: Parser (String, Int)
parse227 = do
    skipWhile (/= '(') *> char '('
    [h1,h2,h3,h4,p1,p2] <- many1 digit `sepBy` char ','
    let host = intercalate "." [h1,h2,h3,h4]
        highBits = read p1
        lowBits = read p2
        portNum = (highBits `shift` 8) + lowBits
    return (host, portNum)

parse257 :: Parser String
parse257 = do
    char '"'
    C.unpack <$> takeTill (== '"')

-- Control commands

login :: MonadIO m => Handle -> String -> String -> m FTPResponse
login h user pass = last <$> sendCommands h [User user, Pass pass]

pasv :: MonadIO m => Handle -> m (String, Int)
pasv h = do
    resp <- sendCommand h Pasv
    let (Right (host, portNum)) = parseOnly parse227 (frMessage resp)
    return (host, portNum)

port :: MonadIO m => Handle -> S.HostAddress -> S.PortNumber -> m FTPResponse
port h ha pn = sendCommand h (Port ha pn)

acct :: MonadIO m => Handle -> String -> m FTPResponse
acct h pass = sendCommand h (Acct pass)

rename :: MonadIO m => Handle -> String -> String -> m FTPResponse
rename h from to = do
    res <- sendCommand h (Rnfr from)
    case frStatus res of
        Continue -> sendCommand h (Rnto to)
        _ -> return res

dele :: MonadIO m => Handle -> String -> m FTPResponse
dele h file = sendCommand h (Dele file)

cwd :: MonadIO m => Handle -> String -> m FTPResponse
cwd h dir =
    sendCommand h $ if dir == ".."
        then Cdup
        else Cwd dir

size :: MonadIO m => Handle -> String -> m Int
size h file = do
    resp <- sendCommand h (Size file)
    return $ read $ C.unpack $ frMessage resp

mkd :: MonadIO m => Handle -> String -> m String
mkd h dir = do
    resp <- sendCommand h (Mkd dir)
    let (Right dir) = parseOnly parse257 (frMessage resp)
    return dir

rmd :: MonadIO m => Handle -> String -> m FTPResponse
rmd h dir = sendCommand h (Rmd dir)

pwd :: MonadIO m => Handle -> m String
pwd h = do
    resp <- sendCommand h Pwd
    let (Right dir) = parseOnly parse257 (frMessage resp)
    return dir

quit :: MonadIO m => Handle -> m FTPResponse
quit h = sendCommand h Quit

-- TLS commands

pbsz :: MonadIO m => Handle -> Int -> m FTPResponse
pbsz h = sendCommand h . Pbsz

prot :: MonadIO m => Handle -> ProtType -> m FTPResponse
prot h = sendCommand h . Prot

ccc :: MonadIO m => Handle -> m FTPResponse
ccc h = sendCommand h Ccc

auth :: MonadIO m => Handle -> m FTPResponse
auth h = sendCommand h Auth

-- Data commands

sendType :: MonadIO m => RTypeCode -> ByteString -> Handle -> m ()
sendType TA dat h = void $ mapM (sendCommandLine h) $ C.split '\n' dat
sendType TI dat h = liftIO $ send h dat

withDataCommandSecurity
    :: (MonadIO m, MonadMask m)
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> (Handle -> m a)
    -> m a
withDataCommandSecurity h =
    case security h of
        Clear -> withDataCommand h
        TLS -> withTLSDataCommand h

nlst :: (MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
nlst h args = withDataCommandSecurity h Passive [RType TA, Nlst args] getAllLineResp

retr :: (MonadIO m, MonadMask m) => Handle -> String -> m ByteString
retr h path = withDataCommandSecurity h Passive [RType TI, Retr path] recvAll

list :: (MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
list h args = withDataCommandSecurity h Passive [RType TA, List args] getAllLineResp

stor
    :: (MonadIO m, MonadMask m)
    => Handle
    -> String
    -> B.ByteString
    -> RTypeCode
    -> m ()
stor h loc dat rtype =
    withDataCommandSecurity h Passive [RType rtype, Stor loc]
        $ sendType rtype dat

data MlsdResponse = MlsdResponse {
    mrFilename :: String,
    mrFacts :: Map String String
} deriving (Show)

splitApart :: Char -> ByteString -> (ByteString, ByteString)
splitApart on s =
    let (x0, x1) = C.break (== on) s
    in (x0, C.drop 1 x1)

parseMlsdLine :: ByteString -> MlsdResponse
parseMlsdLine line =
    let (factLine, filename) = splitApart ' ' line
        bFacts = splitApart '=' <$> C.split ';' factLine
        facts
            = Map.fromList
            $ filter (not . null . fst)
            $ join (***) C.unpack <$> bFacts
    in MlsdResponse (C.unpack filename) facts

getMlsdResponse :: (MonadIO m, MonadCatch m) => Handle -> m [MlsdResponse]
getMlsdResponse h = getMlsdResponse' h []
    where
        getMlsdResponse' h ret = ( do
            line <- liftIO $ getLineResp h
            getMlsdResponse' h $
                if C.null line
                    then ret
                    else (parseMlsdLine line):ret
            ) `M.catchIOError` (\_ -> return ret)

mlsd :: (MonadIO m, MonadMask m) => Handle -> String -> m [MlsdResponse]
mlsd h path = withDataCommandSecurity h Passive [RType TA, Mlsd path] getMlsdResponse
