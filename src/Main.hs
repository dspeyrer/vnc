module Main where

import Control.Exception
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
import Data.Functor
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
    handleClients server

handleClients :: Socket -> IO ()
handleClients server = do
    (msg, exception) <- handle
        (\(e :: SomeException) -> return (displayException e, Just e))
        $ bracket (accept server) (close . fst) $ \(sock, addr) -> do
            putStrLn $ "Connected to " ++ show addr
            stream <- N.getContents sock
            (result, _) <- runProto handleClient $ Client sock stream
            close sock
            return (either id id result, Nothing)
    putStrLn $ "Disconnected -- " ++ msg
    if elem UserInterrupt $ exception >>= fromException
        then return ()
        else handleClients server

handleClient :: Proto a
handleClient = protoVersion

-- ProtocolVersion ---------------------

versionMsg :: StrictByteString
versionMsg = fromString "RFB 003.003\n"

protoVersion :: Proto a
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

protoVerFail :: Proto a
protoVerFail = do
    lift $ put secTyInvalid
    lift $ put unsupportedVersionMsg
    fail "unsupported client version"

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

protoVncAuth :: Proto a
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

protoSecResFail :: Proto a
protoSecResFail = do
    lift $ put secResFail
    fail "authentication failure"

protoSecResOk :: Proto a
protoSecResOk = do
    lift $ put secResOk
    protoClientInit

-- ClientInit -----------------------

protoClientInit :: Proto a
protoClientInit = do
    _share <- lift $ getBool
    protoServerInit

-- ServerInit -----------------------

putServerInitMsg :: String -> PixelFormat -> Put
putServerInitMsg name pf = do
    putWord16be 1080 -- framebuffer-width
    putWord16be  720 -- framebuffer-height
    putPixelFormat pf
    putWord32be      $
        fromIntegral $
        length name
    putStringUtf8 name

protoServerInit :: Proto a
protoServerInit = do
    lift $ putServerInitMsg "headless" $ pixelFormat state
    protoLoop state
    where state = ServerState defaultPixelFormat

-- Main Loop ------------------------

type Pixel = (Word16, Word16, Word16)
type Framebuffer = [[Pixel]]

getFramebuffer :: Framebuffer
getFramebuffer = error "todo"

encodePixel :: PixelFormat -> Pixel -> Put
encodePixel format = error "todo"

encodePixels :: PixelFormat -> Framebuffer -> Put
encodePixels format = mapM_ $ mapM_ $ encodePixel format

protoLoop :: ServerState -> Proto a
protoLoop state = lift getClientMessage >>= \msg -> do
    case msg of
        SetPixelFormat pixelFormat ->
            protoLoop state { pixelFormat }
        FramebufferUpdateRequest _incremental x y w h -> do
            noise <- getStdRandom $ uniformByteString $ div (fromIntegral w * fromIntegral h * (fromIntegral $ bpp $ pixelFormat state)) 8
            lift $ do
                putWord8 0
                putWord8 0
                putWord16be 1

                putWord16be x
                putWord16be y
                putWord16be w
                putWord16be h
                putInt32be 0

                putByteString noise
            protoLoop state
        _ -> do
            lift $ putStrLn $ "Unhandled " ++ show msg
            protoLoop state

data ServerState = ServerState
    { pixelFormat :: PixelFormat
    }

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

defaultPixelFormat :: PixelFormat
defaultPixelFormat = PixelFormat 32 24 True True 255 255 255 16 8 0

putPixelFormat :: PixelFormat -> Put
putPixelFormat f = do
    putWord8    $ bpp        f
    putWord8    $ depth      f
    putBool     $ bigEndian  f
    putBool     $ trueColour f
    putWord16be $ redMax     f
    putWord16be $ greenMax   f
    putWord16be $ blueMax    f
    putWord8    $ redShift   f
    putWord8    $ greenShift f
    putWord8    $ blueShift  f
    putZeroes   3

getPixelFormat :: Get PixelFormat
getPixelFormat = PixelFormat
    <$> getWord8
    <*> getWord8
    <*> getBool
    <*> getBool
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord8
    <*> getWord8
    <*> getWord8
    <* skip 3

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
    | ClientCutText StrictByteString
    deriving (Show) 

getClientMessage :: Get ClientMessage
getClientMessage = getWord8 >>= \ty ->
    case ty of
        0 -> SetPixelFormat
            <$  skip 3
            <*> getPixelFormat
        2 -> SetEncodings
            <$  skip 1
            <*> (getWord16be
                <&> fromIntegral
                >>= flip replicateM getKnownEncoding
                <&> catMaybes)
        3 -> FramebufferUpdateRequest
            <$> getBool
            <*> getWord16be
            <*> getWord16be
            <*> getWord16be
            <*> getWord16be
        4 -> KeyEvent
            <$> getBool
            <*  skip 2
            <*> getWord32be
        5 -> PointerEvent
            <$> getWord8
            <*> getWord16be
            <*> getWord16be
        6 -> ClientCutText
            <$  skip 3
            <*> (getWord32be
                <&> fromIntegral
                >>= getByteString)
        _ -> fail "unknown message type"

-- FramebufferUpdate
-- SetColourMapEntries
-- ServerCutText