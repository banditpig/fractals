{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
-- The package juicypixels needs to have been installed.
-- try cabal install juicypixels
-- https://hackage.haskell.org/package/JuicyPixels-3.2.8.3

import Data.Complex
import Codec.Picture

type C = Complex Double
type Pnt = (Double, Double)
type Grid a = [[a]]

w :: Int 
w = 500

h :: Int 
h = 500 

-- Complex constants for the Julia set rendering.
fixC1, fixC2, fixC3, fixC4 :: C
fixC1 = (-1.037) :+ 0.17
fixC2 = (-0.52) :+ 0.57 
fixC3 = 0.295 :+ 0.55 
fixC4 = (-0.624) :+ 0.435

juliaConstants :: [(Int, C)]
juliaConstants =[ (1, fixC1), (2, fixC2), (3, fixC3), (4, fixC4)]


--iterate f x == [x, f x, f (f x), ...]
-- in mandel the seed is fixed at 0 
mandel :: C -> [C]
mandel c = iterate (\z -> z * z + c)  (0.0 :+ 0.0)

--iterate f x == [x, f x, f (f x), ...]
-- in julia, for a given fixed constant,  the seed varies
julia :: C ->  C -> [C]
julia fixc  = iterate (\z -> z * z + fixc)  

closeToZero :: C -> Bool
closeToZero z = magnitude z < 5

-- get the pallete value corresponding to the 'depth' at which the value fails the test 
chooseColor  :: [color] -> ( C -> [C]) -> C -> color
chooseColor colors fz z = (colors !!) . length . take n . takeWhile closeToZero $ fz z
  where n = length colors - 1

-- A grid of points that is defined by its  bottom left, top right and the number of subdivision per row and per column
grid :: Int -> Int -> Pnt -> Pnt -> Grid Pnt
grid c r (xmin, ymin) (xmax, ymax ) = [[(x, y) | x <- for c xmin xmax ] | y <- for r ymin ymax ] where
    for :: Int ->Double ->Double ->[Double]
    for n mn mx = take n [mn, mn + delta ..]
     where delta = (mx - mn) / fromIntegral (n - 1)

-- a grid
grid1 :: Grid Pnt
grid1 = grid w h (-1.5, -1.5) (1.5, 1.5)

-- calculate sequences over the grid and for each sequence get an 'end' colour
mapOverGrid :: Grid Pnt -> (C -> [C]) -> [color] -> [[color]]
mapOverGrid grd fz colors = [ [chooseColor colors  fz  (x :+ y) | (x,y) <- row] | row <- grd ]

-- used by juicy pixels to get a pixel value at x, y
gridFunc ::Grid Pnt ->  (C -> [C]) -> Int -> Int -> PixelRGB8
gridFunc grd fz  x y = (mapOverGrid grd fz rgb8 !! y) !! x

generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage
generateImg gf = ImageRGB8 (generateImage gf w h)

-- Juicy pixels type 
rgb8 :: [PixelRGB8]
rgb8 = foldr (\x ac  -> PixelRGB8 x x x  : ac) [] [255, 254..0]

allJulia :: IO()
allJulia = 
    mapM_ makeSaveImage juliaConstants where 
        makeSaveImage :: (Int, C) -> IO()
        makeSaveImage (n, c) = 
         savePngImage name $ generateImg . gridFunc grid1 . julia $ c where 
            name = "imageJ" ++ show n ++ ".png" 

main :: IO ()
main = do
    putStrLn "starting -> please wait..."
    allJulia
    savePngImage "imageM.png" $ generateImg . gridFunc grid1 $ mandel
    putStrLn "Done."
