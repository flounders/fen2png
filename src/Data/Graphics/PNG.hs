module Data.Graphics.PNG (ColorType (..), ImageHeader (..), PaletteEntry (..), makeImage) where

import Codec.Compression.Zlib (compress)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Word

data ImageHeader = ImageHeader
  { width :: !Word32
  , height :: !Word32
  , bitDepth :: !Word8
  , colorType :: !ColorType
  , compressionMethod :: !Word8
  , filterMethod :: !Word8
  , interlaceMethod :: !Word8
  }
  deriving (Show)

data ColorType
  = Grayscale
  | RGBTriple
  | PaletteIndex ![PaletteEntry]
  | GrayscaleAlpha
  | RGBTripleAlpha
  deriving (Show)

data PaletteEntry = PaletteEntry {red :: !Word8, green :: !Word8, blue :: !Word8} deriving (Show)

colorTypeToWord :: ColorType -> Word8
colorTypeToWord Grayscale = 0
colorTypeToWord RGBTriple = 2
colorTypeToWord (PaletteIndex _) = 3
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
              <> BSB.word8 (colorTypeToWord $ colorType iData)
              <> BSB.word8 (compressionMethod iData)
              <> BSB.word8 (filterMethod iData)
              <> BSB.word8 (interlaceMethod iData)
       in Just $ makeChunk (builderToStrict $ BSB.string7 "IHDR") (builderToStrict chunkData)

{-
   PLTE chunks are optional, but critical. These must come before IDAT chunks, but after IHDR.

   PLTE chunks are invalid if their length is not divisible by 3.
-}
makePLTE :: ColorType -> Maybe BS.ByteString
makePLTE (PaletteIndex xs)
  | length xs <= 256 =
      let is = foldMap (\(PaletteEntry r g b) -> builderToStrict (BSB.word8 r <> BSB.word8 g <> BSB.word8 b)) xs
       in Just $ makeChunk (builderToStrict $ BSB.string7 "PLTE") is
  | otherwise = Nothing
makePLTE _ = Nothing

-- Data should be verified and generated correctly outside of this function
makeIDAT :: BS.ByteString -> BS.ByteString
makeIDAT = makeChunk (builderToStrict $ BSB.string7 "IDAT")

-- IEND chunks must come at the end of the file.
makeIEND :: BS.ByteString
makeIEND = makeChunk (builderToStrict $ BSB.string7 "IEND") BS.empty

builderToStrict :: BSB.Builder -> BS.ByteString
builderToStrict = BSL.toStrict . BSB.toLazyByteString

makeImage :: ImageHeader -> BS.ByteString -> Maybe BS.ByteString
makeImage ihdr pixels = do
  hdr <- makeIHDR ihdr
  let dat = makeIDAT . BSL.toStrict . compress $ BSL.fromStrict pixels
      color = colorType ihdr
  palette <- case color of
    (PaletteIndex _) -> makePLTE color
    _otherColorTypes -> pure BS.empty
  pure $ pngSignature <> hdr <> palette <> dat <> makeIEND
