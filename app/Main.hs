module Main where

import Lib
import System.Environment
import Args
import File
import Klusters
import System.Exit
import Data.Maybe
import System.Random
import System.IO
import Control.Exception

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs emptyImcInfos
    if (isNothing args) then putStrLn "Invalid arguments" >> exitWith (ExitFailure 84)
    else return ()
    let vArgs = fromJust args
    g <- newStdGen
    contents <- catch (readFile (getF vArgs)) catchFileError
    let pixels = parsePixels contents
    let klusters = createNKlusters (getN vArgs) $ genNRandomColorsF (getN vArgs) 0 g (defColor)
    mapM_ (\k -> putStr (show k)) (runKMeans pixels klusters (getL vArgs) (getL vArgs + 1))
