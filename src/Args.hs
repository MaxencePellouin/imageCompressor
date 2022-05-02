module Args
    ( 
        ImcInfos,
        parseArgs,
        emptyImcInfos,
        getN,
        getL,
        getF
    ) where

import Data.Maybe

data ImcInfos = ImcInfos Int Float String
instance Show ImcInfos where
    show (ImcInfos n l f) = show f ++ " with " ++ show n ++ " klusters and " ++ show l ++ " convergence"

emptyImcInfos :: ImcInfos
emptyImcInfos = ImcInfos (-1) (-1) ""

getN :: ImcInfos -> Int
getN (ImcInfos n _ _) = n

getL :: ImcInfos -> Float
getL (ImcInfos _ l _) = l

getF :: ImcInfos -> String
getF (ImcInfos _ _ f) = f

parseArgs :: [String] -> ImcInfos -> Maybe ImcInfos
parseArgs [] i = Just i
parseArgs ("-n":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos (read x :: Int) l f)
parseArgs ("-l":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos n (read x :: Float) f)
parseArgs ("-f":x:xs) (ImcInfos n l f) = parseArgs xs (ImcInfos n l x)
parseArgs _ _ = Nothing