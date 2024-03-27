module MyLib (someFunc) where

import Codec.Compression.Zlib (compress)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Word

data ImageHeader = ImageHeader
  { width :: Word32
  , height :: Word32
  , bitDepth :: Word8
  , colorType :: Word8
  , compressionMethod :: Word8
  , filterMethod :: Word8
  , interlaceMethod :: Word8
  }
  deriving (Show)

data ColorType
  = Grayscale
  | RGBTriple
  | PalateIndex
  | GrayscaleAlpha
  | RGBTripleAlpha
  deriving (Show)

colorTypeToWord :: ColorType -> Word8
colorTypeToWord Grayscale = 0
colorTypeToWord RGBTriple = 2
colorTypeToWord PalateIndex = 3
colorTypeToWord GrayscaleAlpha = 4
colorTypeToWord RGBTripleAlpha = 6

pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

{-
   Chunks consist of four fields

   1. Length (which is a 4 byte unsigned integer) and only measures the length of the data segment
   2. Type (also 4 bytes) and is restricted to ASCII alphabet characters of both upper and lower case
   3. Data (variable byte length with 0 being possible)
   4. CRC (4 bytes) is calculated on only the type and data, length is left out
-}
makeChunk :: BS.ByteString -> BS.ByteString -> BS.ByteString
makeChunk chunkType chunkData =
  let chunkLength = BSB.word32BE . fromInteger . toInteger $ BS.length chunkData
      chunkCrc = crc (chunkType <> chunkData)
   in builderToStrict chunkLength <> chunkType <> chunkData <> builderToStrict chunkCrc

crc :: BS.ByteString -> BSB.Builder
crc content =
  let table :: M.Map Word32 Word32
      table = M.map (`f` 8) $ M.fromList [(x, x) | x <- [0 .. 255]]
      f :: Word32 -> Int -> Word32
      f x 0 = x
      f x i
        | x .&. 1 == 0 = f (shiftR x 1) (i - 1)
        | otherwise = f (xor 0xedb88320 (shiftR x 1)) (i - 1)
      updatedCrc :: Word32 -> BS.ByteString -> Word32
      updatedCrc = BS.foldl (\acc x -> xor (shiftR acc 8) $ table M.! (.&.) 0xff (xor acc (fromIntegral x)))
   in BSB.word32BE (xor (updatedCrc 0xffffffff content) 0xffffffff)

-- IHDR chunks must come at the beginning of a file after the PNG signature.
makeIHDR :: ImageHeader -> Maybe BS.ByteString
makeIHDR iData
  | width iData >= 2 ^ (31 :: Int) || height iData >= 2 ^ (31 :: Int) = Nothing
  | otherwise =
      let chunkData =
            BSB.word32BE (width iData)
              <> BSB.word32BE (height iData)
              <> BSB.word8 (bitDepth iData)
              <> BSB.word8 (colorType iData)
              <> BSB.word8 (compressionMethod iData)
              <> BSB.word8 (filterMethod iData)
              <> BSB.word8 (interlaceMethod iData)
       in Just $ makeChunk (builderToStrict $ BSB.string7 "IHDR") (builderToStrict chunkData)

{-
   PLTE chunks are optional, but critical. These must come before IDAT chunks, but after IHDR.

   PLTE chunks are invalid if their length is not divisible by 3.
-}
makePLTE = makeChunk (builderToStrict $ BSB.string7 "PLTE") undefined

-- Data should be verified and generated correctly outside of this function
makeIDAT :: BS.ByteString -> BS.ByteString
makeIDAT = makeChunk (builderToStrict $ BSB.string7 "IDAT")

-- IEND chunks must come at the end of the file.
makeIEND :: BS.ByteString
makeIEND = makeChunk (builderToStrict $ BSB.string7 "IEND") BS.empty

builderToStrict :: BSB.Builder -> BS.ByteString
builderToStrict = BSL.toStrict . BSB.toLazyByteString

rgbTripleToBS :: (Word8, Word8, Word8) -> BS.ByteString
rgbTripleToBS (r, g, b) = BS.pack [r, g, b]

makeImage ihdr pixels = do
  hdr <- makeIHDR ihdr
  let dat = makeIDAT . BSL.toStrict . compress $ BSL.fromStrict pixels
  pure $ pngSignature <> hdr <> dat <> makeIEND

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
 - 0000 - 0x00
 - 0001 - 0x01
 - 0010 - 0x02
 - 0011 - 0x03
 - 0100 - 0x04
 - 0101 - 0x05
 - 0110 - 0x06
 - 0111 - 0x07
 - 1000 - 0x08
 - 1001 - 0x09
 - 1010 - 0x0a
 - 1011 - 0x0b
 - 1100 - 0x0c
 - 1101 - 0x0d
 - 1110 - 0x0e
 - 1111 - 0x0f
 -
 - 0xd7 11010111
 - 0xda 11011010
-}