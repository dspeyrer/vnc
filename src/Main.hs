module Main where

import Control.Monad
import Control.Monad.IO.Class
import DES
import Data.ByteString (StrictByteString, toStrict)
import Data.ByteString.Lazy (LazyByteString, fromStrict)
import Data.ByteString qualified as BS
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bool
import Data.Maybe
import Data.String
import Network.Socket
import Network.Socket.ByteString.Lazy as N
import System.Random

data Client = Client
    { clientSocket :: Socket
    , clientStream :: LazyByteString
    }

newtype Proto a = Proto { runProto :: Client -> IO (Either String a, Client) }

instance Functor Proto where
    fmap = (<*>) . pure

instance Applicative Proto where
    mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x
    pure x = Proto $ pure . (pure x,)

instance Monad Proto where
    m1 >>= m2 = Proto $ \c -> runProto m1 c >>= b'
        where b' ( Right x, c') = runProto (m2 x) c'
              b' (Left err, c') = return (Left err, c')

instance MonadIO Proto where
    liftIO m = Proto $ \c -> m >>= \x -> return (return x, c)

instance MonadFail Proto where
    fail err = Proto $ \c -> return (Left err, c)

class Liftable m where
    lift :: m a -> Proto a

instance Liftable IO where
    lift = liftIO 

instance Liftable Get where
    lift m = join $ Proto $ \c ->
        let (stream, res) = case runGetOrFail m (clientStream c) of
                Left  (rest, _, e) -> (rest,  Left e)
                Right (rest, _, x) -> (rest, Right x)
        in return (return $ either fail return res, c { clientStream = stream })

instance Liftable PutM where
    lift m = Proto $ \c -> do
            sendAll (clientSocket c) d
            return (Right x, c)
        where (x, d) = runPutM m

main :: IO ()
main = do
    server <- socket AF_INET Stream defaultProtocol
    setSocketOption server ReuseAddr 1
    bind server $ SockAddrInet 5900 $ tupleToHostAddress (127, 0, 0, 1)
    listen server 1024
    putStrLn "Listening..."
    forever $ accept server >>= \(sock, _) -> do
        putStrLn "Connected"
        stream <- N.getContents sock
        (result, _) <- runProto handleClient $ Client sock stream
        putStrLn $ case result of
            Left err -> "Connection closed (" ++ err ++ ")"
            _ -> "Disconnected"
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
    _share <- lift $ getBool
    protoServerInit

-- ServerInit -----------------------

putServerInitMsg :: String -> Put
putServerInitMsg name = do
    putWord16be 1080 -- framebuffer-width
    putWord16be  720 -- framebuffer-height
    putPixelFormat $ PixelFormat
        32 24 True True
        255 255 255
         16   8   0
    putWord32be      $
        fromIntegral $
        length name     -- name-length
    putStringUtf8 name  -- name-string

protoServerInit :: Proto ()
protoServerInit = do
    lift $ putServerInitMsg "headless"
    forever $ do
        msg <- lift $ getClientMessage
        lift $ putStrLn $ "Got message: " ++ show msg

-- Main Loop ------------------------

putZeroes :: Int -> Put
putZeroes n = replicateM_ n $ putWord8 0

putBool :: Bool -> Put
putBool = putWord8 . bool 0 1

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

data PixelFormat = PixelFormat
  { bpp        :: Word8
  , depth      :: Word8
  , bigEndian  :: Bool
  , trueColour :: Bool
  , redMax     :: Word16
  , greenMax   :: Word16
  , blueMax    :: Word16
  , redShift   :: Word8
  , greenShift :: Word8
  , blueShift  :: Word8
  }
  deriving (Show)

putPixelFormat :: PixelFormat -> Put
putPixelFormat (PixelFormat bpp depth bigEndian trueColour redMax greenMax blueMax redShift greenShift blueShift) = do
    putWord8    $ bpp
    putWord8    $ depth
    putBool     $ bigEndian
    putBool     $ trueColour
    putWord16be $ redMax
    putWord16be $ greenMax
    putWord16be $ blueMax
    putWord8    $ redShift
    putWord8    $ greenShift
    putWord8    $ blueShift
    putZeroes   3

getPixelFormat :: Get PixelFormat
getPixelFormat = do
    bpp        <- getWord8
    depth      <- getWord8
    bigEndian  <- getBool
    trueColour <- getBool
    redMax     <- getWord16be
    greenMax   <- getWord16be
    blueMax    <- getWord16be
    redShift   <- getWord8
    greenShift <- getWord8
    blueShift  <- getWord8
    skip          3
    return $ PixelFormat bpp depth bigEndian trueColour redMax greenMax blueMax redShift greenShift blueShift

data KnownEncoding = Zlib | ZRLE | DesktopSize | Cursor deriving (Show)

getKnownEncoding :: Get (Maybe KnownEncoding)
getKnownEncoding = getInt32be >>= \ty ->
    return $ case ty of
        6    -> Just Zlib
        16   -> Just ZRLE
        -223 -> Just DesktopSize
        -239 -> Just Cursor
        _    -> Nothing

data ClientMessage
    = SetPixelFormat PixelFormat
    | SetEncodings [KnownEncoding]
    | FramebufferUpdateRequest
        { incremental :: Bool
        , x :: Word16
        , y :: Word16
        , w :: Word16
        , h :: Word16
        }
    | KeyEvent
        { down :: Bool
        , key :: Word32
        }
    | PointerEvent
        { btnMask :: Word8
        , x :: Word16
        , y :: Word16
        }
    | ClientCutText String
    deriving (Show) 

getClientMessage :: Get ClientMessage
getClientMessage = getWord8 >>= \ty ->
    case ty of
        0 -> skip 3 >> SetPixelFormat <$> getPixelFormat
        2 -> skip 1 >> do
            n <- getWord16be
            SetEncodings . catMaybes <$> replicateM (fromIntegral n) getKnownEncoding
        3 -> do
            i <- getBool
            x <- getWord16be
            y <- getWord16be
            w <- getWord16be
            h <- getWord16be
            return $ FramebufferUpdateRequest i x y w h
        4 -> fail "unimplemented KeyEvent"
        5 -> fail "unimplemented PointerEvent"
        6 -> fail "unimplemented ClientCutText"
        _ -> fail "unknown message type"

-- FramebufferUpdate
-- SetColourMapEntries
-- ServerCutText