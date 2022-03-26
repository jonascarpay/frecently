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
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)
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
      loadFrecencies path >>= writeFrecencies path . expire . bump str . decay now
    path : "delete" : (stripWhitespace -> Just str) : [] ->
      loadFrecencies path >>= writeFrecencies path . delete str
    path : "view" : augs ->
      loadFrecencies path >>= putStr . unlines . view . augment (mapMaybe stripWhitespace augs) . expire . decay now
    path : "scores" : augs -> do
      loadFrecencies path >>= printScores . augment (mapMaybe stripWhitespace augs) . expire . decay now
    _ -> die "usage: frecently PATH <bump STR|delete STR|view STR*|scores STR*>"

type Time = Word64

newtype NEString = NEString {unNEString :: String}
  deriving newtype (Eq, Ord, Show, Serialize)

getUTC :: IO Time
getUTC = read <$> readProcess "date" ["+%s"] ""

data Energy = Energy
  { hourly :: Double,
    daily :: Double,
    monthly :: Double
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

instance Semigroup Energy where Energy h d m <> Energy h' d' m' = Energy (h + h') (d + d') (m + m')

data Frecencies = Frecencies
  { lastUpdate :: Time,
    energies :: Map NEString Energy
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

decay :: Time -> Frecencies -> Frecencies
decay tNow (Frecencies tUpdate frecs) = Frecencies tNow (fmap f frecs)
  where
    f (Energy h d m) = Energy (alphaHour * h) (alphaDay * d) (alphaMonth * m)
    deltaSec = fromIntegral $ tNow - tUpdate
    alphaHour = 0.5 ** (deltaSec / 3600)
    alphaDay = 0.5 ** (deltaSec / 86400)
    alphaMonth = 0.5 ** (deltaSec / 2592000)

score :: Energy -> Double
score (Energy h d m) = h + d + m

delete :: NEString -> Frecencies -> Frecencies
delete str (Frecencies t fs) = Frecencies t (Map.delete str fs)

expire :: Frecencies -> Frecencies
expire (Frecencies t fs) = Frecencies t (Map.filter ((> 0.2) . score) fs)

bump :: NEString -> Frecencies -> Frecencies
bump str (Frecencies t fs) = Frecencies t (Map.insertWith (<>) str (Energy 720 30 1) fs)

augment :: [NEString] -> Frecencies -> Frecencies
augment strs (Frecencies t fs) = Frecencies t (foldr (\str -> Map.insertWith (<>) str (Energy 0 0 0)) fs strs)

view :: Frecencies -> [String]
view = fmap (unNEString . fst) . sortOn (negate . score . snd) . Map.toList . energies

stripWhitespace :: String -> Maybe NEString
stripWhitespace str = if null str' then Nothing else Just (NEString str)
  where
    str' = (dropWhileEnd isSpace . dropWhile isSpace) str

loadFrecencies :: FilePath -> IO Frecencies
loadFrecencies fp = do
  exists <- doesFileExist fp
  if exists
    then BS.readFile fp >>= either die pure . decode
    else pure $ Frecencies 0 mempty

writeFrecencies :: FilePath -> Frecencies -> IO ()
writeFrecencies path fs = atomicWriteFile path (encode fs)

printScores :: Frecencies -> IO ()
printScores (Frecencies _ fs) = do
  printf "total\t\thourly\t\tdaily\t\tmonthly\n"
  forM_ (sortOn (negate . score . snd) $ Map.toList fs) $ \(str, Energy h d m) -> do
    printf "%12.6f\t%12.6f\t%12.6f\5%12.6f\t%s\n" (h + d + m) h d m (unNEString str)
