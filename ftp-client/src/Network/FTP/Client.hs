module Network.FTP.Client (
    withFTP,
    login,
    pasv,
    nlst,
    createTransferHandlePasv,
    sendCommand
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

sendCommand :: Handle -> FTPCommand -> IO C.ByteString
sendCommand h fc = do
    let command = serializeCommand fc
    print $ "Sending: " <> command
    C.hPut h $ C.pack $ command <> "\r\n"
    resp <- getMultiLineResp h
    print $ "Recieved: " <> resp
    return resp

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
withHandle host port f = bracket (createHandle host port) hClose f

withFTP :: String -> Int -> (Handle -> C.ByteString -> IO a) -> IO a
withFTP host port f = withHandle host port $ \h -> getMultiLineResp h >>= f h

createTransferHandlePasv :: Handle -> IO Handle
createTransferHandlePasv ch = do
    (host, port) <- pasv ch
    print $ "Host: " <> host
    print $ "Port: " <> show port
    th <- createHandle host port
    return th

login :: Handle -> String -> String -> IO C.ByteString
login h user pass = do
    sendCommand h (User user)
    sendCommand h (Pass pass)

pasv :: Handle -> IO (String, Int)
pasv h = do
    resp <- sendCommand h Pasv
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


nlst :: Handle -> [String] -> IO C.ByteString
nlst h args = do
    th <- createTransferHandlePasv h
    print =<< sendCommand h (RType TA)
    print =<< sendCommand h (Nlst args)
    getAllLineResp th
