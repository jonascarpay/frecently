{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Serialize (Serialize, decode, encode)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.Process (readProcess)
import Text.Printf (printf)

main :: IO ()
main = do
  now <- getUTC
  getArgs >>= \case
    path : "bump" : (stripWhitespace -> Just str) : [] ->
      loadFrecencies path >>= writeFrecencies path . expire now . bump now str
    path : "delete" : (stripWhitespace -> Just str) : [] ->
      loadFrecencies path >>= writeFrecencies path . Map.delete str
    path : "view" : augs ->
      loadFrecencies path >>= putStr . unlines . view now . augment (mapMaybe stripWhitespace augs)
    path : "scores" : augs -> do
      loadFrecencies path >>= printScores now . augment (mapMaybe stripWhitespace augs)
    _ -> die "usage: frecently PATH <bump STR|delete STR|view STR*|scores STR*>"

type Time = Word64

newtype NEString = NEString {unNEString :: String}
  deriving newtype (Eq, Ord, Show, Serialize)

getUTC :: IO Time
getUTC = read <$> readProcess "date" ["+%s"] ""

data Stats = Stats
  { lastBump :: Time,
    energy :: Double
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

instance Semigroup Stats where Stats l e <> Stats l' e' = Stats (max l l') (e + e')

type Frecencies = Map NEString Stats

halfLife :: Double
halfLife = 30 * 24 * 60 * 60 -- seconds in 30 days

decayFactor :: Time -> Time -> Double
decayFactor tNow tBump =
  let dUpdate = fromIntegral (tNow - tBump)
      halfLives = dUpdate / halfLife
   in 0.5 ** halfLives

recencyBonus :: Time -> Time -> Double
recencyBonus tNow tBump =
  let dBump = fromIntegral $ tNow - tBump
   in 1 / (dBump / halfLife + 0.01)

score :: Time -> Stats -> Double
score tNow (Stats tBump nrg) = recencyBonus tNow tBump + nrg * decayFactor tNow tBump

expire :: Time -> Frecencies -> Frecencies
expire tNow = Map.filter ((> 0.01) . score tNow)

bump :: Time -> NEString -> Frecencies -> Frecencies
bump tNow str = Map.insertWith (<>) str (Stats tNow 1)

augment :: [NEString] -> Frecencies -> Frecencies
augment strs m = foldr (\str -> Map.insertWith (<>) str (Stats 0 0)) m strs

view :: Time -> Frecencies -> [String]
view tNow = fmap (unNEString . fst) . sortOn (negate . snd) . Map.toList . fmap (score tNow)

stripWhitespace :: String -> Maybe NEString
stripWhitespace str = if null str' then Nothing else Just (NEString str)
  where
    str' = (dropWhileEnd isSpace . dropWhile isSpace) str

loadFrecencies :: FilePath -> IO Frecencies
loadFrecencies fp = do
  exists <- doesFileExist fp
  if exists
    then BS.readFile fp >>= either die pure . decode
    else pure mempty

writeFrecencies :: FilePath -> Frecencies -> IO ()
writeFrecencies path fs = BS.writeFile path (encode fs)

printScores :: Time -> Frecencies -> IO ()
printScores tNow fs = do
  printf "total\t\tenergy\t\trecency bonus\ttime stamp\tage\t\tword\n"
  forM_ (sortOn (negate . score tNow . snd) $ Map.toList fs) $ \(str, Stats tBump nrg) -> do
    let n = nrg * decayFactor tNow tBump
        r = recencyBonus tNow tBump
    printf "%12.7f\t%12.7f\t%12.7f\t%10d\t%10d\t%s\n" (n + r) n r tBump (tNow - tBump) (unNEString str)
