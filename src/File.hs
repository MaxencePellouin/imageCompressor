module File
    (
        catchFileError,
        nextChar,
        beforeChar,
        contentsToLines
    ) where

import Control.Exception
import System.Exit

catchFileError :: SomeException -> IO String
catchFileError e = putStrLn "Error reading file" >> exitWith (ExitFailure 84)

contentsToLines :: String -> [String]
contentsToLines = lines

nextChar :: Char -> String -> String
nextChar c [] = []
nextChar c (x:xs) = if c == x then x:xs else nextChar c xs

beforeChar :: Char -> String -> String
beforeChar _ [] = []
beforeChar c (x:xs) = if c == x then [c] else x : beforeChar c xs