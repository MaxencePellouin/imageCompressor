module Klusters
    (
        Klusters,
        createNKlusters,
        getKlusterDiff,
        genNRandomColorsF,
        Color,
        ColorF,
        Pixel,
        PosI,
        defColor
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
    show (Pixel (PosI (x, y)) (Color (r, g, b))) = "(" ++ show x ++ ", " ++
        show y ++ ") (" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")\n"

instance Show Color where
    show (Color (r, g, b)) = "(" ++ show r ++ ", " ++ show g ++ ", " ++
        show b ++ ")"

instance Show ColorF where
    show (ColorF (r, g, b)) = "(" ++ show r ++ ", " ++ show g ++ ", "
        ++ show b ++ ")\n"

instance Show Klusters where
    show (Klusters (ColorF (r, g, b)) _ []) = "--\n" ++ show (ColorF (r, g, b))
        ++ "-\n"
    show (Klusters (ColorF (r, g, b)) _ pixels) = "--\n" ++
        show (ColorF (r, g, b)) ++ "-\n" ++ unlines (map show pixels)

genNRandomColorsF :: Int -> Int -> StdGen -> ColorF -> [ColorF]
genNRandomColorsF 0 _ _ _ = []
genNRandomColorsF n 0 gen (ColorF (r, g, b)) = genNRandomColorsF n 1 g'
    (ColorF (rand * 255, g, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF n 1 gen (ColorF (r, g, b)) = genNRandomColorsF n 2 g'
    (ColorF (r, rand * 255, b))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF n 2 gen (ColorF (r, g, b)) = (ColorF (r, g, rand * 255)):
    genNRandomColorsF (n - 1) 0 g' (ColorF (0, 0, 0))
    where (rand, g') = randomR (0.0, 1.0) gen
genNRandomColorsF _ _ _ _ = []

createNKlusters :: Int -> [ColorF] -> [Klusters]
createNKlusters 0 _ = []
createNKlusters n colors = (Klusters (head colors) (ColorF (0, 0, 0)) []):
    createNKlusters (n - 1) (tail colors)

getColorDiff :: ColorF -> ColorF -> Float
getColorDiff (ColorF (r1, g1, b1)) (ColorF (r2, g2, b2)) = sqrt
    ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

getKlusterDiff :: Klusters -> Float
getKlusterDiff (Klusters a p _) = getColorDiff a p

-- getPastColor :: Klusters -> ColorF
-- getPastColor (Klusters _ (ColorF (r, g, b)) _) = ColorF (r, g, b)

-- getActualColor :: Klusters -> ColorF
-- getActualColor (Klusters (ColorF (r, g, b)) _ _) = ColorF (r, g, b)

meanColor :: [Pixel] -> ColorF
meanColor [] = ColorF (0, 0, 0)
meanColor pixels = ColorF (r / (fromIntegral (length pixels)),
    g / (fromIntegral (length pixels)), b / (fromIntegral (length pixels)))
    where (r, g, b) = foldl (\(r, g, b) (Pixel _ (Color (r', g', b'))) ->
            (r + r', g + g', b + b')) (0, 0, 0) pixels

updateKluster :: Klusters -> Klusters
updateKluster (Klusters a p []) = (Klusters a a [])
updateKluster (Klusters a p pixels) = (Klusters (meanColor pixels) a pixels)

defColor :: ColorF
defColor = ColorF (0, 0, 0)

test :: IO ()
test = do
    g <- newStdGen
    let klusters = createNKlusters 10  $ genNRandomColorsF 10 0 g (ColorF (0, 0, 0))
    mapM_ (\k -> putStr (show k)) klusters