module DES (encrypt) where

import Data.Bits
import Data.List
import Data.Maybe
import Data.Word

newtype Permutation = Permutation [Int]
newtype Word48      = Word48 { val48 :: Word64 }
newtype Word56      = Word56 { val56 :: Word64 }

fromNum :: Int -> [Int] -> Permutation
fromNum w ix = Permutation $ map (w -) $ reverse ix

inverse :: Permutation -> Permutation
inverse (Permutation ix) = Permutation [fromJust $ elemIndex i ix | i <- [0..63]]

apply :: (Bits a, Bits b) => Permutation -> a -> b
apply (Permutation ix) x =
    foldr (flip setBit . snd) zeroBits $ filter (testBit x . fst) $ zip ix [0..]

intoBlocks :: (Num a, Bits a) => Int -> Int -> a -> [a]
intoBlocks w = c
    where m = bit w - 1
          c 0 _ = []
          c n a = a.&.m : c (n - 1) (a .>>. w)

fromBlocks :: (Bits a) => Int -> [a] -> a
fromBlocks w = foldr acc zeroBits
    where acc x a = (a .<<. w) .|. x

reverseBytes :: Word64 -> Word64
reverseBytes = apply $ fromNum 64 $ foldr1 (++) [let j = i * 8 in [j+8,j+7..j+1] | i <- [0..7]]

ip :: Permutation
ip = fromNum 64
    [ 58, 50, 42, 34, 26, 18, 10, 2
    , 60, 52, 44, 36, 28, 20, 12, 4
    , 62, 54, 46, 38, 30, 22, 14, 6
    , 64, 56, 48, 40, 32, 24, 16, 8
    , 57, 49, 41, 33, 25, 17,  9, 1
    , 59, 51, 43, 35, 27, 19, 11, 3
    , 61, 53, 45, 37, 29, 21, 13, 5
    , 63, 55, 47, 39, 31, 23, 15, 7
    ]

fp :: Permutation
fp = inverse ip

e :: Word32 -> Word48
e = Word48 . apply (fromNum 32
    [ 32,  1,  2,  3,  4,  5
    ,  4,  5,  6,  7,  8,  9
    ,  8,  9, 10, 11, 12, 13
    , 12, 13, 14, 15, 16, 17
    , 16, 17, 18, 19, 20, 21
    , 20, 21, 22, 23, 24, 25
    , 24, 25, 26, 27, 28, 29
    , 28, 29, 30, 31, 32,  1
    ])

p :: Word32 -> Word32
p = apply $ fromNum 32
  [ 16,  7, 20, 21
  , 29, 12, 28, 17
  ,  1, 15, 23, 26
  ,  5, 18, 31, 10
  ,  2,  8, 24, 14
  , 32, 27,  3,  9
  , 19, 13, 30,  6
  , 22, 11,  4, 25
  ]

pc1 :: Word64 -> Word56
pc1 = Word56 . apply (fromNum 64
  [ 57, 49, 41, 33, 25, 17,  9
  ,  1, 58, 50, 42, 34, 26, 18
  , 10,  2, 59, 51, 43, 35, 27
  , 19, 11,  3, 60, 52, 44, 36
  , 63, 55, 47, 39, 31, 23, 15
  ,  7, 62, 54, 46, 38, 30, 22
  , 14,  6, 61, 53, 45, 37, 29
  , 21, 13,  5, 28, 20, 12,  4
  ])

pc2 :: Word56 -> Word48
pc2 = Word48 . apply (fromNum 56
  [ 14, 17, 11, 24,  1,  5
  ,  3, 28, 15,  6, 21, 10
  , 23, 19, 12,  4, 26,  8
  , 16,  7, 27, 20, 13,  2
  , 41, 52, 31, 37, 47, 55
  , 30, 40, 51, 45, 33, 48
  , 44, 49, 39, 56, 34, 53
  , 46, 42, 50, 36, 29, 32
  ]) . val56

lshift :: Word56 -> Word56
lshift = Word56 . apply (fromNum 56 $ [2..28] ++ [1] ++ [30..56] ++ [29]) . val56

shiftS :: [Word56 -> Word56]
shiftS = map lshiftn [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 :: Int]
  where lshiftn 0 = id
        lshiftn n = lshiftn (n - 1) . lshift

data KeyData = KeyData
  { sh :: [Word56 -> Word56]
  , cd :: Word56
  }

keyDataInit :: Word64 -> KeyData
keyDataInit = KeyData shiftS . pc1

keyDataNext :: KeyData -> (Word48, KeyData)
keyDataNext prev = (pc2 c'd', KeyData (tail $ sh prev) c'd')
  where c'd' = (head $ sh prev) $ cd prev

sidx :: Word32 -> Word32
sidx = apply $ fromNum 6 [1, 6, 2, 3, 4, 5]

s :: Word48 -> Word32
s = xf $ reverse $ map reorder [
    [ 14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7
    , 0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8
    , 4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0
    , 15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13
    ]
  , [ 15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10
    , 3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5
    , 0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15
    , 13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9
    ]
  , [ 10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8
    , 13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1
    , 13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7
    , 1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12
    ]
  , [ 7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15
    , 13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9
    , 10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4
    , 3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14
    ]
  , [ 2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9
    , 14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6
    , 4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14
    , 11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3
    ]
  , [ 12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11
    , 10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8
    , 9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6
    , 4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13
    ]
  , [ 4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1
    , 13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6
    , 1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2
    , 6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12
    ]
  , [ 13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7
    , 1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2
    , 7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8
    , 2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11
    ]
  ]
  where
    xf boxes = fromBlocks 4 . zipWith (\b -> (!!) b . fromIntegral) boxes . intoBlocks 6 8 . val48
    reorder box = zipWith (\_ -> (!!) box . fromIntegral . sidx) box [0..]

f :: Word32 -> Word48 -> Word32
f r k = p $ s b
  where b = Word48 $ val48 k `xor` (val48 $ e r)

split :: Word64 -> (Word32, Word32)
split x = (fromIntegral $ shiftR x 32, fromIntegral x)

join :: (Word32, Word32) -> Word64
join (l, r) = shiftL (fromIntegral l) 32 .|. fromIntegral r

iter1 :: (Word32, Word32) -> KeyData -> ((Word32, Word32), KeyData)
iter1 (l, r) k = ((r, l `xor` f r ks), k')
  where (ks, k') = keyDataNext k

compute :: Word64 -> (Word32, Word32) -> (Word32, Word32)
compute k x = fst $ foldl iter (x, keyDataInit k) [1..16 :: Int]
  where iter (lr, kd) _ = iter1 lr kd

swap :: (a, b) -> (b, a)
swap = uncurry $ flip (,)

preoutput :: Word64 -> Word64 -> Word64
preoutput k = join . swap . compute k . split

encrypt :: Word64 -> Word64 -> Word64
encrypt k = apply fp . preoutput (reverseBytes k) . apply ip
