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
import Windows

data ClientCtx = ClientCtx
    { interface :: Interface
    , clientSocket :: Socket
    }

data ClientState = ClientState
    { clientStream :: LazyByteString
    }

newtype Proto a = Proto
    { runProto :: ClientCtx
               -> ClientState
               -> IO (Either String a, ClientState)
    }

instance Functor Proto where
    fmap = (<*>) . pure

instance Applicative Proto where
    mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x
    pure x = Proto $ const $ pure . (pure x,)

instance Monad Proto where
    m1 >>= m2 = Proto $ \i c -> runProto m1 i c >>= b' i
        where b' i (Right  x, c') = runProto (m2 x) i c'
              b' _ (Left err, c') = return (Left err, c')

getInterface :: Proto Interface
getInterface = Proto $ \c s -> return (Right $ interface c, s)

instance MonadIO Proto where
    liftIO m = Proto $ \_ s -> m >>= \x -> return (return x, s)

instance MonadFail Proto where
    fail err = Proto $ \_ s -> return (Left err, s)

class Liftable m where
    lift :: m a -> Proto a

instance Liftable IO where
    lift = liftIO 

-- Should probably be written in terms of setState
instance Liftable Get where
    lift m = join $ Proto $ \_ s ->
        let (stream, res) = case runGetOrFail m (clientStream s) of
                Left  (rest, _, e) -> (rest,  Left e)
                Right (rest, _, x) -> (rest, Right x)
        in return (return $ either fail return res, s { clientStream = stream })

instance Liftable PutM where
    lift m = Proto $ \c s -> do
            sendAll (clientSocket c) d
            return (Right x, s)
        where (x, d) = runPutM m

main :: IO ()
main = do
    server <- socket AF_INET Stream defaultProtocol
    setSocketOption server ReuseAddr 1
    bind server $ SockAddrInet 5900 $ tupleToHostAddress (0, 0, 0, 0)
    listen server 1024
    putStrLn "Listening..."
    interface <- createInterface
    handleClients interface server

handleClients :: Interface -> Socket -> IO ()
handleClients interface server = do
    (msg, exception) <- handle
        (\(e :: SomeException) -> return (displayException e, Just e))
        $ bracket (accept server) (close . fst) $ \(sock, addr) -> do
            putStrLn $ "Connected to " ++ show addr
            stream <- N.getContents sock
            (result, _) <- runProto handleClient
                (ClientCtx interface sock)
                (ClientState stream)
            close sock
            return (either id id result, Nothing)
    putStrLn $ "Disconnected -- " ++ msg
    if elem UserInterrupt $ exception >>= fromException
        then return ()
        else handleClients interface server


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

putServerInitMsg :: Word16 -> Word16 -> String -> PixelFormat -> Put
putServerInitMsg w h name pf = do
    putWord16be w
    putWord16be h
    putPixelFormat pf
    putWord32be      $
        fromIntegral $
        length name
    putStringUtf8 name

protoServerInit :: Proto a
protoServerInit = do
    i <- getInterface
    let w = fromIntegral $ capW i
        h = fromIntegral $ capH i
    lift $ putServerInitMsg w h "surface" $ pixelFormat state
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
        FramebufferUpdateRequest _ _ _ _ _ -> do
            i <- getInterface

            let w = fromIntegral $ capW i
                h = fromIntegral $ capH i

            img <- lift $ capture i

            lift $ do
                putWord8    0 -- ty
                putWord8    0 -- padding
                putWord16be 1 -- # rectangles

                putWord16be 0 -- x
                putWord16be 0 -- y
                putWord16be w -- w
                putWord16be h -- h
                putInt32be  0 -- encoding

                putByteString img

            protoLoop state
        _ -> do
            lift $ putStrLn $ "Unhandled " ++ show msg
            protoLoop state

-- Maybe this should be lifted to the Proto monad?
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
            <*> (fromIntegral
                <$> getWord16be
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
            <*> (fromIntegral
                <$> getWord32be
                >>= getByteString)
        _ -> fail "unknown message type"

-- FramebufferUpdate
-- SetColourMapEntries
-- ServerCutText