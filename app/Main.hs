{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Serialize (Serialize, decode, encode)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Options.Applicative hiding (str)
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  now <- getUTC
  cmd <- execParser (info pCommand mempty)
  case cmd of
    Bump fa str thresh weights -> withFrecencies fa $ expire thresh weights . bump str . decay now
    Delete fa str -> withFrecencies fa $ delete str
    View fa va weights -> do
      frecs <- loadFrecencies fa
      set <- readInput
      putStr . unlines . view weights . augment va set . decay now $ frecs
    Scores fa va weights -> do
      frecs <- loadFrecencies fa
      set <- readInput
      printScores weights . augment va set . decay now $ frecs

pCommand :: Parser Command
pCommand =
  hsubparser (command "bump" $ info (Bump <$> pFileArgs <*> pStringArg <*> pExpireArgs <*> pWeights) mempty)
    <|> hsubparser (command "view" $ info (View <$> pFileArgs <*> pAugmentArgs <*> pWeights) mempty)
    <|> hsubparser (command "delete" $ info (Delete <$> pFileArgs <*> pStringArg) mempty)
    <|> hsubparser (command "scores" $ info (Scores <$> pFileArgs <*> pAugmentArgs <*> pWeights) mempty)

data Command
  = Bump FileArgs NEString ExpireArgs Weights
  | View FileArgs AugmentArgs Weights
  | Delete FileArgs NEString
  | Scores FileArgs AugmentArgs Weights

data FileArgs = FileArgs
  { faPath :: FilePath,
    _faErrorIfMissing :: Bool
  }

newtype Weights = Weights Energy

pFileArgs :: Parser FileArgs
pFileArgs =
  FileArgs
    <$> strOption (long "history-file" <> short 'f' <> help "History file to use")
    <*> flag False True (long "missing-file-error" <> help "Throw an error if the file is missing, instead of treating it as an empty history.")

pStringArg :: Parser NEString
pStringArg = argument (maybeReader $ \str -> guard (notElem '\n' str) >> stripWhitespace str) (metavar "ARG")

pWeights :: Parser Weights
pWeights = option (Weights <$> reader) (long "weights" <> short 'w' <> help "Weights in hourly,daily,monthly format. Default: 720,24,1.")
  where
    reader = maybeReader $ \str -> do
      case splitOn ',' str of
        [m, d, h] -> liftA3 Energy (readMaybe m) (readMaybe d) (readMaybe h)
        _ -> Nothing
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn sep = go
      where
        go [] = []
        go str =
          let (h, t) = span (/= sep) str
           in h : go (drop 1 t)

newtype ExpireArgs = ExpireArgs {_uaThreshold :: Double}

pExpireArgs :: Parser ExpireArgs
pExpireArgs =
  ExpireArgs
    <$> option
      auto
      ( long "threshold"
          <> short 't'
          <> help "Expiration threshold. Items with a score lower than this will be removed."
          <> value 0.2
          <> showDefault
      )

readInput :: IO [NEString]
readInput = mapMaybe stripWhitespace . lines <$> getContents

newtype AugmentArgs = AugmentArgs {_vaRestrict :: Bool}

pAugmentArgs :: Parser AugmentArgs
pAugmentArgs = AugmentArgs <$> flag False True (long "restrict" <> short 'r' <> help "Restrictive mode. Will only output items that are in the list read from stdin.")

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

score :: Weights -> Energy -> Double
score (Weights (Energy wh wd wm)) (Energy h d m) = wh * h + wd * d + wm * m

delete :: NEString -> Frecencies -> Frecencies
delete str (Frecencies t fs) = Frecencies t (Map.delete str fs)

expire :: ExpireArgs -> Weights -> Frecencies -> Frecencies
expire (ExpireArgs threshold) weights (Frecencies t fs) = Frecencies t (Map.filter ((> threshold) . score weights) fs)

-- TODO bump by 1 1 1, multiply scores before presenting
bump :: NEString -> Frecencies -> Frecencies
bump str (Frecencies t fs) = Frecencies t (Map.insertWith (<>) str (Energy 720 30 1) fs)

augment :: AugmentArgs -> [NEString] -> Frecencies -> Frecencies
augment (AugmentArgs restrict) strs (Frecencies t fs) =
  Frecencies t $
    if restrict
      then Map.fromList $ (\key -> maybe (key, Energy 0 0 0) (key,) (Map.lookup key fs)) <$> strs
      else foldr (\str -> Map.insertWith (<>) str (Energy 0 0 0)) fs strs

view :: Weights -> Frecencies -> [String]
view weights = fmap (unNEString . fst) . sortOn (negate . score weights . snd) . Map.toList . energies

stripWhitespace :: String -> Maybe NEString
stripWhitespace str = if null str' then Nothing else Just (NEString str)
  where
    str' = (dropWhileEnd isSpace . dropWhile isSpace) str

loadFrecencies :: FileArgs -> IO Frecencies
loadFrecencies (FileArgs fp errorOnMissing) = do
  exists <- doesFileExist fp
  if exists
    then BS.readFile fp >>= either die pure . decode
    else
      if errorOnMissing
        then die $ "Error: missing history file " <> fp
        else pure $ Frecencies 0 mempty

withFrecencies :: FileArgs -> (Frecencies -> Frecencies) -> IO ()
withFrecencies fa f = loadFrecencies fa >>= writeFrecencies (faPath fa) . f

writeFrecencies :: FilePath -> Frecencies -> IO ()
writeFrecencies path fs = atomicWriteFile path (encode fs)

printScores :: Weights -> Frecencies -> IO ()
printScores weights (Frecencies _ fs) = do
  printf "total\t\thourly\t\tdaily\t\tmonthly\n"
  forM_ (sortOn (negate . score weights . snd) $ Map.toList fs) $ \(str, Energy h d m) -> do
    printf "%12.6f\t%12.6f\t%12.6f\5%12.6f\t%s\n" (h + d + m) h d m (unNEString str)
