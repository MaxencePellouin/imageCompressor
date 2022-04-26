module Args
    ( 
        ImcInfos,
        parseArgs,
        emptyImcInfos
    ) where

import Data.Maybe

data ImcInfos = ImcInfos Int Double String
instance Show ImcInfos where
    show (ImcInfos n l f) = show f ++ " with " ++ show n ++ " klusters and " ++ show l ++ " convergence"

emptyImcInfos :: ImcInfos
emptyImcInfos = ImcInfos (-1) (-1) ""

parseArgs :: [String] -> ImcInfos -> Maybe ImcInfos
parseArgs [] i = Just i
parseArgs ("-n":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos (read x :: Int) l f)
parseArgs ("-l":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos n (read x :: Double) f)
parseArgs ("-f":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos n l x)
parseArgs _ _ = Nothing