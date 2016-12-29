module Network.FTP.Client.Conduit (
    nlst
) where

import Data.Conduit.Binary
import Data.Conduit
import Control.Monad.IO.Class
import System.IO
import Network.FTP.Client (createTransferHandlePasv, sendCommand, FTPCommand(..), RTypeCode(..))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

nlst :: MonadIO m => Handle -> [String] -> Producer m ByteString
nlst h args = do
        th <- liftIO $ do
            th <- createTransferHandlePasv h
            print =<< sendCommand h (RType TA)
            print =<< sendCommand h (Nlst args)
            return th
        sourceHandle th
