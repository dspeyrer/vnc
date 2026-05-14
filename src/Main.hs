module Main where

import Control.Monad
import DES
import Data.ByteString as BS
import Data.Binary
import Data.String
import Network.Socket
import Network.Socket.ByteString as NS
import Network.Socket.ByteString.Lazy as NL
import System.Random

main :: IO ()
main = do
    server <- socket AF_INET Stream defaultProtocol
    setSocketOption server ReuseAddr 1
    bind server $ SockAddrInet 5900 $ tupleToHostAddress (127, 0, 0, 1)
    listen server 1024
    putStrLn "Listening..."
    forever $ accept server >>= \(client, _) -> handleClient client *> close client

handleClient :: Socket -> IO ()
handleClient s = do
    putStrLn "Accepted connection"
    protoVersion s

-- ProtocolVersion ---------------------

versionMsg :: StrictByteString
versionMsg = fromString "RFB 003.003\n"

protoVersion :: Socket -> IO ()
protoVersion s = do
    NS.sendAll s versionMsg
    resVerMsg <- readExact s 12
    (if versionMsg == resVerMsg
       then protoVncAuth
       else protoVerFail) s

unsupportedVersionMsg :: StrictByteString
unsupportedVersionMsg = fromString "Unsupported client version!"

-- Security ----------------------------

secTyInvalid :: Word32
secTyNone    :: Word32
secTyVncAuth :: Word32

secTyInvalid = 0
secTyNone    = 1
secTyVncAuth = 2

protoVerFail :: Socket -> IO ()
protoVerFail s = do
    write secTyInvalid s
    NS.sendAll s unsupportedVersionMsg

protoAuthNone :: Socket -> IO ()
protoAuthNone s = do
    write secTyNone s
    protoClientInit s

password :: Word64
password = decode $ fromString "password"

solveChallenge :: StrictByteString -> StrictByteString
solveChallenge x =
    uncurry BS.append
        $ both (toStrict . encode . DES.encrypt password . decode . fromStrict)
        $ BS.splitAt 8 x
    where both f (a, b) = (f a, f b)

protoVncAuth :: Socket -> IO ()
protoVncAuth s = do
    write secTyVncAuth s
    challenge <- getStdRandom (uniformByteString 16)
    NS.sendAll s challenge
    res <- readExact s 16
    (if res == solveChallenge challenge
        then protoSecResOk
        else protoSecResFail) s

-- SecurityResult ----------------------

secResOk   :: Word32
secResFail :: Word32

secResOk   = 0
secResFail = 1

protoSecResFail :: Socket -> IO ()
protoSecResFail s = do
    write secResFail s

protoSecResOk :: Socket -> IO ()
protoSecResOk s = do
    write secResOk s
    protoClientInit s

-- ClientInit -----------------------

protoClientInit :: Socket -> IO ()
protoClientInit s = do
    -- TODO
    return ()

-- Rest -----------------------------

write :: Binary a => a -> Socket -> IO ()
write x s = NL.sendAll s $ encode x

readExact :: Socket -> Int -> IO StrictByteString
readExact _ 0 = return BS.empty
readExact stream n = do
    h <- NS.recv stream n
    if BS.null h then
        error "unexpected EOF"
    else
        readExact stream (n - BS.length h)
            >>= (\t -> return $ BS.append h t)
