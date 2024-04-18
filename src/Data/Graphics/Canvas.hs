module Data.Graphics.Canvas where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)

data Canvas = Canvas {canvas :: IOVector Scanline, height :: Int, width :: Int}

newtype Scanline = Scanline {scanline :: IOVector Pixel}

data Pixel = Pixel {red :: Word8, green :: Word8, blue :: Word8}

newCanvas :: Int -> Int -> IO Canvas
newCanvas w h = do
  c <- MV.replicateM h . fmap Scanline . MV.replicate w $ Pixel 0 0 0
  return $ Canvas c h w

putPixel :: Int -> Int -> Pixel -> Canvas -> IO ()
putPixel x y p c
  | x >= width c || y >= height c = fail "Canvas: Pixel location is out of bounds of the canvas."
  | otherwise = MV.modifyM (canvas c) (\l -> MV.modify (scanline l) (const p) x >> return l) y

canvasToPNGBytes :: Canvas -> IO ByteString
canvasToPNGBytes = do
  let lineToBS = MV.foldl' (\acc (Pixel r g b) -> BS.append acc $ BS.pack [r, g, b]) (BS.singleton 0)
  MV.foldM' (\acc x -> BS.append acc <$> lineToBS (scanline x)) BS.empty . canvas
