module Klusters
    (
        genNRandomColorsF,
        Klusters
    ) where
    
import System.Random
import System.IO.Unsafe
import Data.Maybe

data Color = Color (Int, Int, Int)
data PosI = PosI (Integer, Integer)
data ColorF = ColorF (Float, Float, Float)
data Pixel = Pixel PosI Color
data Klusters = Klusters ColorF ColorF [Pixel]

instance Show Pixel where
    show (Pixel (PosI (x, y)) (Color (r, g, b))) = "(" ++ show x ++ ", " ++ show y ++ ") (" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

instance Show Color where
    show (Color (r, g, b)) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

instance Show ColorF where
    show (ColorF (r, g, b)) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

genNRandomColorsF :: Int -> Int -> StdGen -> ColorF -> [ColorF]
genNRandomColorsF 0 _ _ _ = []
genNRandomColorsF n 0 gen (ColorF (r, g, b)) = genNRandomColorsF n 1 g' (ColorF (rand * 255, g, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF n 1 gen (ColorF (r, g, b)) = genNRandomColorsF n 2 g' (ColorF (r, rand * 255, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF n 2 gen (ColorF (r, g, b)) = (ColorF (r, g, rand * 255)):genNRandomColorsF (n - 1) 0 g' (ColorF (0, 0, 0))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF _ _ _ _ = []

getColorDiff :: ColorF -> ColorF -> Float
getColorDiff (ColorF (r1, g1, b1)) (ColorF (r2, g2, b2)) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

test :: IO ()
test = do
    g <- newStdGen
    let colors = genNRandomColorsF 10 0 g (ColorF (0, 0, 0))
    print colors
    let diff = getColorDiff (head colors) (last colors)
    print diff