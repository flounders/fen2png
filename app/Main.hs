module Main where

import qualified Data.ByteString as BS
import Data.Graphics.PNG

-- Example use of our library until I flesh it out more
board :: Maybe BS.ByteString
board =
  let color = PaletteIndex [PaletteEntry 255 0 0, PaletteEntry 0 0 255]
      imageHeader = ImageHeader 80 80 8 color 0 0 0
      row1 = replicate 40 0 <> replicate 40 1
      row2 = replicate 40 1 <> replicate 40 0
      pixels = foldMap (\x -> BS.pack $ 0 : x) $ replicate 40 row1 <> replicate 40 row2
   in makeImage imageHeader pixels

main :: IO ()
main = do
  maybe (putStrLn "Function failed.") (BS.writeFile "test.png") board
