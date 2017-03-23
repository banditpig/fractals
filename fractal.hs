{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Data.Complex
import Codec.Picture

type C = Complex Double
type Pnt = (Double, Double)
type Grid a = [[a]]

w :: Int 
w = 500
h :: Int 
h = 500

--iterate f x == [x, f x, f (f x), ...]
fofz :: (C -> C) -> C -> [C]
fofz  = iterate  

mandel :: C ->  [C]
mandel   = fofz (\z -> z * z * z + 0.395)  ---0.1+0.651*i

close :: C -> Bool
close z = magnitude z < 10

--z3 + 0.400 - good
chooseColor  :: [color] -> ( C -> [C]) -> C -> color
chooseColor palette frac z = (palette !!) . length . take n . takeWhile close $ frac z
  where n = length palette -  1

grid :: Int -> Int -> Pnt -> Pnt -> Grid Pnt
grid c r (xmin, ymin) (xmax, ymax ) = [[(x, y) | x <- for c xmin xmax ] | y <- for r ymin ymax ] where
    for :: Int ->Double ->Double ->[Double]
    for n mn mx = take n [mn, mn + delta ..]
     where delta = (mx - mn) / fromIntegral (n - 1)

grid1 :: Grid Pnt
grid1 = grid w h (-2.0, -2.0) (2.0, 2.0)

mapOverGrid :: [color] -> [[color]]
mapOverGrid  colors = [ [chooseColor colors  mandel  (x :+ y) | (x,y) <- row] | row <- grid1 ]

generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage
generateImg gf = ImageRGB8 (generateImage gf w h)

intColors :: [Int]
intColors = [0..255]

gridF :: [color] -> Int -> Int -> color
gridF colors x y = ((mapOverGrid colors) !! y) !! x


gridFunc :: Int -> Int -> PixelRGB8
gridFunc x y = PixelRGB8 rgb rgb rgb where rgb = fromIntegral $ 255 -  ((mapOverGrid intColors) !! y) !! x

gridFunc1 :: Int -> Int -> PixelRGB8
gridFunc1 x y = ((mapOverGrid rgb8) !! y) !! x

rgb8 :: [PixelRGB8]
rgb8 = foldr (\x ac  -> PixelRGB8 x x x : ac) [] [255, 254..0]


main :: IO ()
main = savePngImage "/Users/mikehoughton/haskell/book/image.png" $ generateImg  gridFunc1




