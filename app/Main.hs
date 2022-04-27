module Main where

import Lib
import System.Environment
import Args
import Klusters
import System.Exit
import Data.Maybe
import System.Random

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs emptyImcInfos
    if (isNothing args) then putStrLn "Invalid arguments" >> exitWith (ExitFailure 84)
    else return ()
    let vArgs = fromJust args
    g <- newStdGen
    let klusters = createNKlusters (getN vArgs) $ genNRandomColorsF (getN vArgs) 0 g (defColor)
    mapM_ (\k -> putStr (show k)) klusters
