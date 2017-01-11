module Network.FTP.Client (
    withFTP,
    withFTPS,
    login,
    pasv,
    nlst,
    retr,
    stor,
    rename,
    dele,
    cwd,
    size,
    mkd,
    rmd,
    pwd,
    quit,
    nlstS,
    createDataSocket,
    sendCommand,
    sendCommands,
    FTPCommand(..),
    RTypeCode(..),
    Handle(..),
    getLineResp,
    PortActivity(..),
    createSendDataCommand,
    getMultiLineResp,
    sendCommandLine,
    sIOHandleImpl
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
import Control.Monad
import Data.Bits
import Network.Connection
import System.IO.Error
import Data.ByteString.Lazy.Internal (defaultChunkSize)

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

data Handle = Handle
    { send :: ByteString -> IO ()
    , sendLine :: ByteString -> IO ()
    , recv :: Int -> IO ByteString
    , recvLine :: IO ByteString
    }

data FTPResponse = FTPResponse {
    frStatus :: ResponseStatus,
    frCode :: Int,
    frMessage :: ByteString
}

instance Show FTPResponse where
    show fr = (show $ frCode fr) <> " " <> (C.unpack $ frMessage fr)

data ResponseStatus
    = Wait
    | Success
    | Continue
    | FailureRetry
    | Failure
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

getLineResp :: Handle -> IO ByteString
getLineResp h = stripCLRF <$> recvLine h

getMultiLineResp :: Handle -> IO FTPResponse
getMultiLineResp h = do
    line <- getLineResp h
    let (code, rest) = C.splitAt 3 line
    message <- if C.head rest == '-'
        then loopMultiLine h code line
        else return line
    return $ FTPResponse
        (responseStatus code)
        (read $ C.unpack code)
        (C.drop 4 message)

loopMultiLine :: Handle -> ByteString -> ByteString -> IO ByteString
loopMultiLine h code line = do
    nextLine <- getLineResp h
    let multiLine = line <> "\n" <> nextLine
        nextCode = C.take 3 nextLine
    if nextCode == code
        then return multiLine
        else loopMultiLine h nextCode multiLine

sendCommandLine :: Handle -> ByteString -> IO ()
sendCommandLine h dat = send h $ dat <> "\r\n"

sendCommand :: Handle -> FTPCommand -> IO FTPResponse
sendCommand h fc = do
    let command = serializeCommand fc
    print $ "Sending: " <> command
    sendCommandLine h $ C.pack command
    resp <- getMultiLineResp h
    print $ "Recieved: " <> (show resp)
    return resp

sendCommands :: Handle -> [FTPCommand] -> IO [FTPResponse]
sendCommands = mapM . sendCommand

-- Control connection

createSocket :: Maybe String -> Int -> S.AddrInfo -> IO (S.Socket, S.AddrInfo)
createSocket host portNum hints = do
    addr:_ <- S.getAddrInfo (Just hints) host (Just $ show portNum)
    print $ "Addr: " <> show addr
    sock <- S.socket
        (S.addrFamily addr)
        (S.addrSocketType addr)
        (S.addrProtocol addr)
    return (sock, addr)

createSocketPassive :: String -> Int -> IO S.Socket
createSocketPassive host portNum = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream
    }
    (sock, addr) <- createSocket (Just host) portNum hints
    S.connect sock (S.addrAddress addr)
    print "Connected"
    return sock

createSocketActive :: IO S.Socket
createSocketActive = do
    let hints = S.defaultHints {
        S.addrSocketType = S.Stream,
        S.addrFlags = [S.AI_PASSIVE]
    }
    (sock, addr) <- createSocket Nothing 0 hints
    S.bind sock (S.addrAddress addr)
    S.listen sock 1
    print "Listening"
    return sock

createSIOHandle :: String -> Int -> IO SIO.Handle
createSIOHandle host portNum = do
    sock <- createSocketPassive host portNum
    S.socketToHandle sock SIO.ReadWriteMode

sIOHandleImpl :: SIO.Handle -> Handle
sIOHandleImpl h = Handle
    { send = C.hPut h
    , sendLine = C.hPutStrLn h
    , recv = C.hGetSome h
    , recvLine = C.hGetLine h
    }

withSIOHandle :: String -> Int -> (Handle -> IO a) -> IO a
withSIOHandle host portNum f = bracket
    (createSIOHandle host portNum)
    SIO.hClose
    (f . sIOHandleImpl)


withFTP :: String -> Int -> (Handle -> FTPResponse -> IO a) -> IO a
withFTP host portNum f = withSIOHandle host portNum $ \h -> do
    resp <- getMultiLineResp h
    f h resp

-- Data connection

createDataSocketPasv :: Handle -> IO S.Socket
createDataSocketPasv h = do
    (host, portNum) <- pasv h
    print $ "Host: " <> host
    print $ "Port: " <> show portNum
    createSocketPassive host portNum

createDataSocketActive :: Handle -> IO S.Socket
createDataSocketActive h = do
    socket <- createSocketActive
    (S.SockAddrInet sPort sHost) <- S.getSocketName socket
    port h sHost sPort
    return socket

createDataSocket :: PortActivity -> Handle -> IO S.Socket
createDataSocket Active  = createDataSocketActive
createDataSocket Passive = createDataSocketPasv

acceptData :: S.Socket -> PortActivity -> IO S.Socket
acceptData sock Passive = return sock
acceptData sock Active = do
    (socket, _) <- S.accept sock
    return socket

createSendDataCommand
    :: Handle
    -> PortActivity
    -> [FTPCommand]
    -> IO SIO.Handle
createSendDataCommand h pa cmds = do
    socket <- createDataSocket pa h
    sendCommands h cmds
    acceptedSock <- acceptData socket pa
    S.socketToHandle acceptedSock SIO.ReadWriteMode

withDataCommand
    :: Show a
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> (Handle -> IO a)
    -> IO a
withDataCommand ch pa cmds f = do
    x <- bracket
        (createSendDataCommand ch pa cmds)
        SIO.hClose
        (f . sIOHandleImpl)
    resp <- getMultiLineResp ch
    print $ "Recieved: " <> (show resp)
    return x

getAllLineResp :: Handle -> IO ByteString
getAllLineResp h = getAllLineResp' h []
    where
        getAllLineResp' h ret = (do
            line <- getLineResp h
            getAllLineResp' h (ret <> [line]))
                `catchIOError` (\_ -> return $ C.intercalate "\n" ret)

recvAll :: Handle -> IO ByteString
recvAll h = recvAll' ""
    where
        recvAll' bs = (do
            chunk <- recv h defaultChunkSize
            recvAll' $ bs <> chunk)
                `catchIOError` (\_ -> return bs)

-- TLS connection

connectTLS :: SIO.Handle -> String -> Int -> IO Connection
connectTLS h host portNum = do
    context <- initConnectionContext
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
    connectFromHandle context h connectionParams

createTLSConnection :: String -> Int -> IO (FTPResponse, Connection)
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
    , recv = connectionGetExact c
    , recvLine = connectionGetLine maxBound c
    }

withTLSHandle :: String -> Int -> (Handle -> FTPResponse -> IO a) -> IO a
withTLSHandle host portNum f = bracket
    (createTLSConnection host portNum)
    (\(_, conn) -> connectionClose conn)
    (\(resp, conn) -> f (tlsHandleImpl conn) resp)

withFTPS :: String -> Int -> (Handle -> FTPResponse -> IO a) -> IO a
withFTPS host portNum = withTLSHandle host portNum

-- TLS data connection

createTLSSendDataCommand
    :: Handle
    -> PortActivity
    -> [FTPCommand]
    -> IO Connection
createTLSSendDataCommand ch pa cmds = do
    sendCommands ch [Pbsz 0, Prot P]
    socket <- createDataSocket pa ch
    sendCommands ch cmds
    acceptedSock <- acceptData socket pa
    (S.SockAddrInet sPort sHost) <- S.getSocketName acceptedSock
    let (h1, h2, h3, h4) = S.hostAddressToTuple sHost
        hostName = intercalate "." $ (show . fromEnum) <$> [h1, h2, h3, h4]
    h <- S.socketToHandle acceptedSock SIO.ReadWriteMode
    connectTLS h hostName (fromEnum sPort)

withTLSDataCommand
    :: Show a
    => Handle
    -> PortActivity
    -> [FTPCommand]
    -> (Handle -> IO a)
    -> IO a
withTLSDataCommand ch pa cmds f = do
    x <- bracket
        (createTLSSendDataCommand ch pa cmds)
        connectionClose
        (f . tlsHandleImpl)
    resp <- getMultiLineResp ch
    print $ "Recieved: " <> (show resp)
    return x

-- Control commands

login :: Handle -> String -> String -> IO FTPResponse
login h user pass = last <$> sendCommands h [User user, Pass pass]

pasv :: Handle -> IO (String, Int)
pasv h = do
    resp <- sendCommand h Pasv
    let (Right (host, portNum)) = parseOnly parse227 (frMessage resp)
    return (host, portNum)

port :: Handle -> S.HostAddress -> S.PortNumber -> IO FTPResponse
port h ha pn = sendCommand h (Port ha pn)

acct :: Handle -> String -> IO FTPResponse
acct h pass = sendCommand h (Acct pass)

rename :: Handle -> String -> String -> IO FTPResponse
rename h from to = do
        res <- sendCommand h (Rnfr from)
        case frStatus res of
            Continue -> sendCommand h (Rnto to)
            _ -> return res

dele :: Handle -> String -> IO FTPResponse
dele h file = sendCommand h (Dele file)

cwd :: Handle -> String -> IO FTPResponse
cwd h dir = do
    sendCommand h $ if dir == ".."
        then Cdup
        else Cwd dir

size :: Handle -> String -> IO Int
size h file = do
    resp <- sendCommand h (Size file)
    return $ read $ C.unpack $ frMessage resp

mkd :: Handle -> String -> IO String
mkd h dir = do
    resp <- sendCommand h (Mkd dir)
    let (Right dir) = parseOnly parse257 (frMessage resp)
    return dir

rmd :: Handle -> String -> IO FTPResponse
rmd h dir = sendCommand h (Rmd dir)

pwd :: Handle -> IO String
pwd h = do
    resp <- sendCommand h Pwd
    let (Right dir) = parseOnly parse257 (frMessage resp)
    return dir

quit :: Handle -> IO FTPResponse
quit h = sendCommand h Quit

-- TLS commands

pbsz :: Handle -> Int -> IO FTPResponse
pbsz h = sendCommand h . Pbsz

prot :: Handle -> ProtType -> IO FTPResponse
prot h = sendCommand h . Prot

ccc :: Handle -> IO FTPResponse
ccc h = sendCommand h Ccc

auth :: Handle -> IO FTPResponse
auth h = sendCommand h Auth

-- Data commands

nlst :: Handle -> [String] -> IO ByteString
nlst h args = withDataCommand h Passive [RType TA, Nlst args] getAllLineResp

retr :: Handle -> String -> IO ByteString
retr h path = withDataCommand h Passive [RType TI, Retr path] recvAll

list :: Handle -> [String] -> IO ByteString
list h args = withDataCommand h Passive [RType TA, List args] recvAll

stor :: Handle -> String -> B.ByteString -> RTypeCode -> IO ()
stor h loc dat rtype = do
    withDataCommand h Passive [RType rtype, Stor loc] $ \dh ->
        case rtype of
            TA -> void $ mapM (sendCommandLine dh) $ C.split '\n' dat
            TI -> send dh dat

-- TLS data commands

nlstS :: Handle -> [String] -> IO ByteString
nlstS h args = withTLSDataCommand h Passive [RType TA, Nlst args] getAllLineResp
