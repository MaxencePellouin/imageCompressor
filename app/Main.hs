module Main where

import Lib
import System.Environment
import Args
import Klusters
import System.Exit
import Data.Maybe

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs emptyImcInfos
    if (isNothing args) then putStrLn "Invalid arguments" >> exitWith (ExitFailure 84)
    else return ()
    let vArgs = fromJust args
    print (vArgs)
