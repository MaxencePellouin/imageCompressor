module Main where

import Lib
import System.Environment
import Args
import File
import Klusters
import System.Exit
import Data.Maybe
import System.Random
import Control.Exception
import Control.Monad

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs emptyImcInfos
    when (isNothing args) $ putStrLn "Bad args" >> exitWith (ExitFailure 84)
    let vArgs = fromJust args
    g <- newStdGen
    contents <- catch (readFile (getF vArgs)) catchFileError
    let ks = createNKlusters (getN vArgs) $ genNColF (getN vArgs) 0 g defCol
    mapM_ (putStr . show) (runKMeans (parsePixels contents)
        ks (getL vArgs) (getL vArgs + 1))
