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
      loadFrecencies path >>= writeFrecencies path . expire . bump str . decay now
    path : "delete" : (stripWhitespace -> Just str) : [] ->
      loadFrecencies path >>= writeFrecencies path . delete str
    path : "view" : augs ->
      loadFrecencies path >>= putStr . unlines . view . augment (mapMaybe stripWhitespace augs)
    path : "debug" : augs ->
      loadFrecencies path >>= debugView . augment (mapMaybe stripWhitespace augs) . decay now
    _ -> die "usage: frecently PATH <bump STR|delete STR|view STRS|debug STRS>"

type Time = Word64

type Score = Double

newtype NEString = NEString {unNEString :: String}
  deriving newtype (Eq, Ord, Show, Serialize)

getUTC :: IO Time
getUTC = read <$> readProcess "date" ["+%s"] ""

data Frecencies = Frecencies
  { lastUpdate :: Time,
    frecencies :: Map NEString Score
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

decay :: Time -> Frecencies -> Frecencies
decay now (Frecencies prev m) = Frecencies now ((* α) <$> m)
  where
    α :: Double
    α =
      let halfLife = 30 * 24 * 60 * 60 -- seconds in 30 days
          deltaT = fromIntegral (now - prev)
          halfLives = deltaT / halfLife
       in 0.5 ** halfLives

delete :: NEString -> Frecencies -> Frecencies
delete str (Frecencies t fs) = Frecencies t (Map.delete str fs)

expire :: Frecencies -> Frecencies
expire (Frecencies t fs) = Frecencies t (Map.filter (> 0.5) fs)

bump :: NEString -> Frecencies -> Frecencies
bump str (Frecencies t fs) = Frecencies t (Map.insertWith (+) str 1 fs)

augment :: [NEString] -> Frecencies -> Frecencies
augment strs (Frecencies t fs) = Frecencies t (foldr (\str -> Map.insertWith (\_ old -> old) str 0) fs strs)

view :: Frecencies -> [String]
view = fmap (unNEString . fst) . sortOn (negate . snd) . Map.toList . frecencies

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
writeFrecencies path fs = BS.writeFile path (encode fs)

debugView :: Frecencies -> IO ()
debugView (Frecencies t fs) = do
  readProcess "date" ["-d", '@' : show t] "" >>= putStrLn
  forM_ (sortOn (negate . snd) $ Map.toList fs) $ \(str, score) -> do
    printf "%.5f\t\t%s\n" score (unNEString str)
