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

-- iterate f x == [x, f x, f (f x), ...]
-- iterate :: (a -> a) -> a -> [a]
fixC :: C
fixC = (-1.037) :+ 0.17

mandel :: C -> [C]
mandel c = iterate (\z -> z * z + c)  (0.0 :+ 0.0)

julia :: C ->  C -> [C]
julia fixc c = iterate (\z -> z * z + fixc)  c

julia' :: C -> [C]
julia' = julia fixC

close :: C -> Bool
close z = magnitude z < 5

chooseColor  :: [color] -> ( C -> [C]) -> C -> color
chooseColor colors fz z = (colors !!) . length . take n . takeWhile close $ fz z
  where n = length colors -  1

grid :: Int -> Int -> Pnt -> Pnt -> Grid Pnt
grid c r (xmin, ymin) (xmax, ymax ) = [[(x, y) | x <- for c xmin xmax ] | y <- for r ymin ymax ] where
    for :: Int ->Double ->Double ->[Double]
    for n mn mx = take n [mn, mn + delta ..]
     where delta = (mx - mn) / fromIntegral (n - 1)

grid1 :: Grid Pnt
grid1 = grid w h (-1.5, -1.5) (1.5, 1.5)

mapOverGrid :: (C -> [C]) -> [color] -> [[color]]
mapOverGrid fz colors = [ [chooseColor colors  fz  (x :+ y) | (x,y) <- row] | row <- grid1 ]

generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage
generateImg gf = ImageRGB8 (generateImage gf w h)

gridFunc :: (C -> [C]) -> Int -> Int -> PixelRGB8
gridFunc fz  x y = ((mapOverGrid fz rgb8) !! y) !! x

rgb8 :: [PixelRGB8]
rgb8 = foldr (\x ac  -> PixelRGB8 x x x  : ac) [] [255, 254..0]

rgb16 :: [PixelRGB16]
rgb16 = foldr (\x ac  -> PixelRGB16 x x x : ac) [] [511, 510  ..0]


main :: IO ()

main = savePngImage "/Users/mikehoughton/haskell/book/image.png" $ generateImg . gridFunc $ mandel

