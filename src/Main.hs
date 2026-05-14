module Main where

import Control.Monad
import Control.Monad.IO.Class
import DES
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.String
import Network.Socket
import Network.Socket.ByteString.Lazy as N
import System.Random

data Client = Client
    { clientSocket :: Socket
    , clientStream :: LazyByteString
    }

newtype Proto a = Proto { runProto :: Client -> IO (Maybe a, Client) }

instance Functor Proto where
    fmap = (<*>) . pure

instance Applicative Proto where
    mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x
    pure x = Proto $ pure . (pure x,)

instance Monad Proto where
    m1 >>= m2 = Proto $ \c -> runProto m1 c >>= b'
        where b' ( Just x, c') = runProto (m2 x) c'
              b' (Nothing, c') = return (Nothing, c')

instance MonadIO Proto where
    liftIO m = Proto $ \c -> m >>= \x -> return (return x, c)

class Liftable m where
    lift :: m a -> Proto a

instance Liftable IO where
    lift = liftIO 

instance Liftable Get where
    lift m = Proto $ \c ->
        return $ case runGetOrFail m (clientStream c) of
            Left _ ->
                (Nothing, c)
            Right (rest, _, x) ->
                (Just x, c { clientStream = rest })

instance Liftable PutM where
    lift m = Proto $ \c -> do
            sendAll (clientSocket c) d
            return (Just x, c)
        where (x, d) = runPutM m

main :: IO ()
main = do
    server <- socket AF_INET Stream defaultProtocol
    setSocketOption server ReuseAddr 1
    bind server $ SockAddrInet 5900 $ tupleToHostAddress (127, 0, 0, 1)
    listen server 1024
    putStrLn "Listening..."
    forever $ accept server >>= \(sock, _) -> do
        stream <- N.getContents sock
        _ <- runProto handleClient $ Client sock stream
        close sock


handleClient :: Proto ()
handleClient = protoVersion

-- ProtocolVersion ---------------------

versionMsg :: StrictByteString
versionMsg = fromString "RFB 003.003\n"

protoVersion :: Proto ()
protoVersion = do
    lift $ putByteString versionMsg
    resVerMsg <- lift $ getByteString 12
    if versionMsg == resVerMsg
       then protoVncAuth
       else protoVerFail

unsupportedVersionMsg :: StrictByteString
unsupportedVersionMsg = fromString "Unsupported client version!"

-- Security ----------------------------

secTyInvalid :: Word32
secTyNone    :: Word32
secTyVncAuth :: Word32

secTyInvalid = 0
secTyNone    = 1
secTyVncAuth = 2

protoVerFail :: Proto ()
protoVerFail = do
    lift $ put secTyInvalid
    lift $ put unsupportedVersionMsg

protoAuthNone :: Proto ()
protoAuthNone = do
    lift $ put secTyNone
    protoClientInit

password :: Word64
password = decode $ fromString "password"

solveChallenge :: StrictByteString -> StrictByteString
solveChallenge x =
    uncurry BS.append
        $ both (toStrict . encode . DES.encrypt password . decode . fromStrict)
        $ BS.splitAt 8 x
    where both f (a, b) = (f a, f b)

protoVncAuth :: Proto ()
protoVncAuth = do
    lift $ put secTyVncAuth
    challenge <- getStdRandom (uniformByteString 16)
    lift $ putByteString challenge
    res <- lift $ getByteString 16
    if res == solveChallenge challenge
        then protoSecResOk
        else protoSecResFail

-- SecurityResult ----------------------

secResOk   :: Word32
secResFail :: Word32

secResOk   = 0
secResFail = 1

protoSecResFail :: Proto ()
protoSecResFail = do
    lift $ put secResFail

protoSecResOk :: Proto ()
protoSecResOk = do
    lift $ put secResOk
    protoClientInit

-- ClientInit -----------------------

protoClientInit :: Proto ()
protoClientInit = do
    -- TODO
    return ()