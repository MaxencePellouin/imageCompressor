module Klusters
    (
        Klusters,
        createNKlusters,
        getKlusterDiff,
        genNColF,
        Color,
        ColorF,
        Pixel,
        PosI,
        defCol,
        parsePixels,
        lineToPixel,
        addPixelsToKlusters,
        runKMeans
    ) where

import System.Random
import Data.Maybe
import Data.List
import File

newtype Color = Color (Int, Int, Int)
newtype PosI = PosI (Integer, Integer)
newtype ColorF = ColorF (Float, Float, Float)
data Pixel = Pixel PosI Color
data Klusters = Klusters ColorF ColorF [Pixel]

instance Show Pixel where
    show (Pixel (PosI (x, y)) (Color (r, g, b))) = "(" ++ show x ++ "," ++
        show y ++ ") (" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Show Color where
    show (Color (r, g, b)) = "(" ++ show r ++ "," ++ show g ++ "," ++
        show b ++ ")"

instance Show ColorF where
    show (ColorF (r, g, b)) = "(" ++ show (round r) ++ "," ++
        show (round g) ++ "," ++ show (round b) ++ ")\n"

instance Show Klusters where
    show (Klusters (ColorF (r, g, b)) _ []) = "--\n"
        ++ show (ColorF (r, g, b)) ++ "-\n"
    show (Klusters (ColorF (r, g, b)) _ pixels) = "--\n" ++
        show (ColorF (r, g, b)) ++ "-\n" ++ unlines (map show pixels)

genNColF :: Int -> Int -> StdGen -> ColorF -> [ColorF]
genNColF 0 _ _ _ = []
genNColF n 0 gen (ColorF (r, g, b)) = genNColF n 1 g'
    (ColorF (rand * 255, g, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNColF n 1 gen (ColorF (r, g, b)) = genNColF n 2 g'
    (ColorF (r, rand * 255, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNColF n 2 gen (ColorF (r, g, b)) = ColorF (r, g, rand * 255):
    genNColF (n - 1) 0 g' (ColorF (0, 0, 0))
    where (rand, g') = randomR (0.0, 1.0) gen
genNColF _ _ _ _ = []

createNKlusters :: Int -> [ColorF] -> [Klusters]
createNKlusters 0 _ = []
createNKlusters n colors = Klusters (head colors) (ColorF (0, 0, 0)) []:
    createNKlusters (n - 1) (tail colors)

getColorDiff :: ColorF -> ColorF -> Float
getColorDiff (ColorF (r1, g1, b1)) (ColorF (r2, g2, b2)) = sqrt
    ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

getKlusterDiff :: Klusters -> Float
getKlusterDiff (Klusters a p _) = getColorDiff a p

-- ! Might need

getPastColor :: Klusters -> ColorF
getPastColor (Klusters _ (ColorF (r, g, b)) _) = ColorF (r, g, b)

getActualColor :: Klusters -> ColorF
getActualColor (Klusters (ColorF (r, g, b)) _ _) = ColorF (r, g, b)

--  *

meanColor :: [Pixel] -> ColorF
meanColor [] = ColorF (0, 0, 0)
meanColor pixels = ColorF (fromIntegral r / fromIntegral (length pixels),
    fromIntegral g / fromIntegral (length pixels), fromIntegral b /
    fromIntegral (length pixels))
    where (r, g, b) = foldl (\(r, g, b) (Pixel _ (Color (r', g', b'))) ->
            (r + r', g + g', b + b')) (0, 0, 0) pixels

updateKluster :: Klusters -> Klusters
updateKluster (Klusters a p []) = Klusters a a []
updateKluster (Klusters a p pixels) = Klusters (meanColor pixels) a pixels

clearKluster :: Klusters -> Klusters
clearKluster (Klusters a p _) = Klusters a p []

defCol :: ColorF
defCol = ColorF (0, 0, 0)

getColorFromThreeInts :: (Int, Int, Int) -> Color
getColorFromThreeInts (r, g, b) = Color (r, g, b)

lineToPixel :: String -> Pixel
lineToPixel line = Pixel position color
    where
        positionn = read (beforeChar ')' line) :: (Int, Int)
        colorr = read (nextChar '(' (tail line)) :: (Int, Int, Int)
        position = PosI
            (fromIntegral (fst positionn), fromIntegral (snd positionn))
        color = getColorFromThreeInts colorr

parsePixels :: String -> [Pixel]
parsePixels = map lineToPixel . contentsToLines

addPixelsToKlusters :: [Pixel] -> [Klusters] -> [Klusters]
addPixelsToKlusters ps k = foldl (flip addPixelToKluster) k ps

addPixelToKluster :: Pixel -> [Klusters] -> [Klusters]
addPixelToKluster _ [] = []
addPixelToKluster (Pixel x col) ks =
    addPixelToKluster' (Pixel x col) ks
        (fromJust (elemIndex (foldl1' min dst) dst))
        where dst = map (getColorDiff (colorToColorF col) . getActualColor) ks

addPixelToKluster' :: Pixel -> [Klusters] -> Int -> [Klusters]
addPixelToKluster' _ [] _ = []
addPixelToKluster' pxl ((Klusters a p pixels):ks) i =
    if i == 0 then Klusters a p (pxl:pixels) : ks
    else Klusters a p pixels : addPixelToKluster' pxl ks (i - 1)

colorToColorF :: Color -> ColorF
colorToColorF (Color (r, g, b)) = ColorF
    (fromIntegral r, fromIntegral g, fromIntegral b)

runKMeans :: [Pixel] -> [Klusters] -> Float -> Float -> [Klusters]
runKMeans pxls ks c min = if min < c then ks
    else  runKMeans pxls spreadedPix c
        (foldl1' max (map getKlusterDiff spreadedPix))
    where
        spreadedPix = spreadPixels pxls (map clearKluster ks)

spreadPixels :: [Pixel] -> [Klusters] -> [Klusters]
spreadPixels [] ks = ks
spreadPixels pxls ks = map updateKluster addedPixels
    where
        addedPixels = addPixelsToKlusters pxls ks