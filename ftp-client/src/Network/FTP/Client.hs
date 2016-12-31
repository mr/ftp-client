module Network.FTP.Client (
    withFTP,
    login,
    pasv,
    nlst,
    createDataConnectionPasv,
    sendCommand,
    sendCommands,
    FTPCommand(..),
    RTypeCode(..),
    ControlConnection(..),
    ccClose,
    DataConnection(..),
    dcClose,
    getLineResp
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.List
import Data.Attoparsec.ByteString.Char8
import Network.Socket
import System.IO
import Data.Monoid ((<>))
import Control.Exception
import Control.Monad
import Data.Bits

import Debug.Trace

parse227 :: Parser (String, Int)
parse227 = do
    string "227"
    skipWhile (/= '(') *> char '('
    [h1,h2,h3,h4,p1,p2] <- many1 digit `sepBy` char ','
    let host = intercalate "." [h1,h2,h3,h4]
        highBits = read p1
        lowBits = read p2
        port = (highBits `shift` 8) + lowBits
    return (host, port)

newtype ControlConnection = CC Handle
newtype DataConnection = DC Handle

ccClose :: ControlConnection -> IO ()
ccClose (CC h) = hClose h

dcClose :: DataConnection -> IO ()
dcClose (DC h) = hClose h

data ResponseStatus
    = Wait
    | Success
    | Continue
    | FailureRetry
    | Failure

responseStatus :: C.ByteString -> ResponseStatus
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

data FTPCommand
    = User String
    | Pass String
    | Acct String
    | RType RTypeCode
    | Retr String
    | Nlst [String]
    | Abor
    | Pasv

serializeCommand :: FTPCommand -> String
serializeCommand (User user) = "USER " <> user
serializeCommand (Pass pass) = "PASS " <> pass
serializeCommand (Acct acct) = "ACCT " <> acct
serializeCommand (RType rt)  = "TYPE " <> serialzeRTypeCode rt
serializeCommand (Retr file) = "RETR " <> file
serializeCommand (Nlst [])   = "NLST"
serializeCommand (Nlst args) = "NLST " <> intercalate " " args
serializeCommand Abor        = "ABOR"
serializeCommand Pasv        = "PASV"

stripCLRF = C.takeWhile $ (&&) <$> (/= '\r') <*> (/= '\n')

getLineResp :: Handle -> IO C.ByteString
getLineResp h = stripCLRF <$> C.hGetLine h

getMultiLineResp :: Handle -> IO C.ByteString
getMultiLineResp h = do
    line <- getLineResp h
    let (code, rest) = C.splitAt 3 line
    if C.head rest == '-'
        then loopMultiLine h code line
        else return line

loopMultiLine :: Handle -> C.ByteString -> C.ByteString -> IO C.ByteString
loopMultiLine h code line = do
    nextLine <- getLineResp h
    let multiLine = line <> "\n" <> nextLine
        nextCode = C.take 3 nextLine
    if nextCode == code
        then return multiLine
        else loopMultiLine h nextCode multiLine

sendCommand :: ControlConnection -> FTPCommand -> IO C.ByteString
sendCommand (CC h) fc = do
    let command = serializeCommand fc
    print $ "Sending: " <> command
    C.hPut h $ C.pack $ command <> "\r\n"
    resp <- getMultiLineResp h
    print $ "Recieved: " <> resp
    return resp

sendCommands :: ControlConnection -> [FTPCommand] -> IO [C.ByteString]
sendCommands cc = mapM (sendCommand cc)

createHandle :: String -> Int -> IO Handle
createHandle host port = do
    let hints = defaultHints {
        addrFlags = [AI_NUMERICSERV],
        addrSocketType = Stream
    }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    print $ "Addr: " <> show addr

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    print "Connected"
    socketToHandle sock ReadWriteMode

withHandle :: String -> Int -> (Handle -> IO a) -> IO a
withHandle host port = bracket (createHandle host port) hClose

withFTP :: String -> Int -> (ControlConnection -> C.ByteString -> IO a) -> IO a
withFTP host port f = withHandle host port $ \h -> getMultiLineResp h >>= f (CC h)

createDataConnectionPasv :: ControlConnection -> IO DataConnection
createDataConnectionPasv ch = do
    (host, port) <- pasv ch
    print $ "Host: " <> host
    print $ "Port: " <> show port
    th <- createHandle host port
    return $ DC th

withDataConnectionPasv :: ControlConnection -> (DataConnection -> IO a) -> IO a
withDataConnectionPasv cc = bracket (createDataConnectionPasv cc) (\(DC h) -> hClose h)

login :: ControlConnection -> String -> String -> IO C.ByteString
login cc user pass = do
    sendCommand cc (User user)
    sendCommand cc (Pass pass)

pasv :: ControlConnection -> IO (String, Int)
pasv cc = do
    resp <- sendCommand cc Pasv
    let (Right (host, port)) = parseOnly parse227 resp
    return (host, port)

getAllLineResp :: Handle -> IO C.ByteString
getAllLineResp h = getAllLineResp' h []
    where
        getAllLineResp' h ret = do
            eof <- hIsEOF h
            if eof
                then return $ C.intercalate "\n" ret
                else do
                    line <- getLineResp h
                    getAllLineResp' h (ret <> [line])

nlst :: ControlConnection -> [String] -> IO C.ByteString
nlst cc args = withDataConnectionPasv cc $ \(DC h) -> do
    sendCommands cc [RType TA, Nlst args]
    getAllLineResp h
