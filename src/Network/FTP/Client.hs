module Network.FTP.Client (
    testFTP
) where

import qualified Data.ByteString.Char8 as C
import Network.Socket
import System.IO
import Data.Monoid ((<>))

hostName = "ftp.ftp.ftp"
port = "21"
gUser = "dank"
gPass = "memes"

data FTPCommand
    = User String
    | Pass String
    | Acct String
    | Abor

serializeCommand :: FTPCommand -> String
serializeCommand (User user) = "USER " <> user
serializeCommand (Pass pass) = "PASS " <> pass
serializeCommand (Acct acct) = "ACCT " <> acct
serializeCommand (Abor)      = "ABOR"

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
    C.hPut h $ C.pack $ serializeCommand fc <> "\r\n"
    getMultiLineResp h

login :: Handle -> String -> String -> IO C.ByteString
login h user pass = do
    sendCommand h (User user)
    sendCommand h (Pass pass)


testFTP :: IO ()
testFTP = do
    let hints = defaultHints {
        addrFlags = [AI_NUMERICSERV],
        addrSocketType = Stream
    }
    addr:_ <- getAddrInfo (Just hints) (Just hostName) (Just port)

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    h <- socketToHandle sock ReadWriteMode
    -- Welcome!
    print =<< getMultiLineResp h

    print =<< login h gUser gPass

    hClose h
