module Windows (Interface, createInterface, capture, destroyInterface, capW, capH) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString (StrictByteString, toStrict)
import Data.ByteString.Internal (create)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Binary
import Data.Binary.Put
import Data.Int
import Foreign.Ptr
import Graphics.Win32.Window
import Graphics.Win32.GDI.Bitmap
import Graphics.Win32.GDI.HDC
import Graphics.Win32.GDI.Graphics2D
import Graphics.Win32.GDI.Types

putZeroes :: Int -> Put
putZeroes n = replicateM_ n $ putWord8 0

-- Probably fine, I'm sure
foreign import ccall unsafe "windows.h SetProcessDPIAware" setProcessDPIAware :: IO ()

-- "Safety First!"
dibBitmapInfo :: Int32 -> Int32 -> StrictByteString
dibBitmapInfo w h = toStrict $ runPut $ do
    let header = toStrict $ runPut $ do
            -- DWORD biSize
            putWord32host sizeofBITMAPINFO
            -- LONG  biWidth
            putInt32host w
            -- LONG  biHeight
            putInt32host (-h)
            -- WORD  biPlanes
            putWord16host 1
            -- WORD  biBitCount
            putWord16host 32
            -- u32 DWORD biCompression
            putWord32host bI_RGB
    putByteString header
    -- And there's a few other fields or something, whatever
    putZeroes $ fromIntegral (fromIntegral sizeofBITMAPINFO - BS.length header)

data Interface = Interface
    { capW :: Int32
    , capH :: Int32
    , size :: Int
    , srcDC :: HDC
    , tgtDC :: HDC
    , bmp :: HBITMAP
    , bitmapInfo :: StrictByteString
    }

createInterface :: IO Interface
createInterface = do
    setProcessDPIAware
    d <- getDesktopWindow
    hdc <- getWindowDC $ Just d
    (l, t, r, b) <- getWindowRect d
    tgtDC <- createCompatibleDC $ Just hdc
    let w = r - l
    let h = b - t
    tgtBmp <- createCompatibleBitmap hdc w h
    selectBitmap tgtDC tgtBmp >>= deleteBitmap
    let bitmapInfo = dibBitmapInfo w h
    let size = fromIntegral $ 4 * w * h
    return $ Interface w h size hdc tgtDC tgtBmp bitmapInfo

capture :: Interface -> IO StrictByteString
capture (Interface w h size srcDC tgtDC bmp bitmapInfo) = do
    bitBlt tgtDC 0 0 w h srcDC 0 0 sRCCOPY
    unsafeUseAsCString bitmapInfo $
        \inf -> create size       $
        \buf -> void              $
        getDIBits tgtDC bmp 0 h (Just $ castPtr buf) (castPtr inf) dIB_RGB_COLORS

destroyInterface :: Interface -> IO ()
destroyInterface c = deleteBitmap (bmp c) >> deleteDC (tgtDC c)