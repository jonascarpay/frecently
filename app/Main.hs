{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Serialize (Serialize, decode, encode)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Options.Applicative hiding (str)
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (die)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import Text.Printf (printf)

main :: IO ()
main = do
  now <- getUTC
  cmd <- customExecParser (prefs $ showHelpOnEmpty <> showHelpOnError) (info (pCommand <**> helper) mempty)
  case cmd of
    Bump str thresh fa -> withFrecencies fa $ expire thresh . bump str . decay now
    Delete str fa -> withFrecencies fa $ delete str
    View augmentArgs weights fa -> do
      frecs <- loadFrecencies fa
      fAugment <- augment augmentArgs
      putStr . unlines . view weights . fAugment . decay now $ frecs
    Scores augmentArgs weights fa -> do
      frecs <- loadFrecencies fa
      fAugment <- augment augmentArgs
      printScores weights . fAugment . decay now $ frecs
    Touch expireArgs fileArgs -> do
      withFrecencies fileArgs $ expire expireArgs . decay now

data Command
  = Bump NEString ExpireArgs FileArgs
  | View AugmentArgs Weights FileArgs
  | Delete NEString FileArgs
  | Scores AugmentArgs Weights FileArgs
  | Touch ExpireArgs FileArgs

pCommand :: Parser Command
pCommand =
  subparser $
    command
      "bump"
      ( info
          (withFile $ Bump <$> pStringArg (help "The entry to bump") <*> pExpireArgs)
          ( progDesc "Bump a single entry and update the database"
              <> footer "Bumping adds 1 to the entry's hourly/weekly/monthly energy, updates all entries' energy, and removes entries whose monthly energy has dropped below the threshold."
          )
      )
      <> command
        "view"
        ( info
            (withFile $ View <$> pAugmentArgs <*> pWeights)
            ( progDesc "View the history"
                <> footer "When used with --augment and/or --restrict, this blocks on stdin."
            )
        )
      <> command
        "delete"
        ( info
            (withFile $ Delete <$> pStringArg (help "The entry to delete"))
            (progDesc "Delete an entry from the history")
        )
      <> command
        "scores"
        ( info
            (withFile $ Scores <$> pAugmentArgs <*> pWeights)
            ( progDesc "View score table"
                <> footer "The hourly/daily/weekly energies are presented unweighted. Supports the same stdin functionality as the view command."
            )
        )
      <> command
        "touch"
        ( info
            (withFile $ Touch <$> pExpireArgs)
            (progDesc "Create and/or update a history file")
        )

withFile :: Parser (FileArgs -> a) -> Parser a
withFile inner =
  (\fp a err -> a (FileArgs fp err))
    <$> strArgument (help "History file to use" <> metavar "FILE")
    <*> inner
    <*> flag False True (long "missing-file-error" <> short 'e' <> help "Throw an error if the file is missing, instead of treating it as an empty history")

data FileArgs = FileArgs
  { faPath :: FilePath,
    _faErrorIfMissing :: Bool
    -- TODO different output file
  }

newtype Weights = Weights Energy

pStringArg :: Mod ArgumentFields NEString -> Parser NEString
pStringArg extraInfo = argument (maybeReader $ \str -> guard ('\n' `notElem` str) >> stripWhitespace str) (metavar "KEY" <> extraInfo)

pWeights :: Parser Weights
pWeights =
  fmap Weights $
    Energy
      <$> option auto (short 'h' <> long "hourly" <> metavar "FLOAT" <> help "Hourly energy weight" <> showDefault <> value 720)
      <*> option auto (short 'd' <> long "daily" <> metavar "FLOAT" <> help "Daily energy weight" <> showDefault <> value 30)
      <*> option auto (short 'm' <> long "monthly" <> metavar "FLOAT" <> help "Monthly energy weight" <> showDefault <> value 1)

-- TODO no-decay
newtype ExpireArgs = ExpireArgs {_uaThreshold :: Double}

pExpireArgs :: Parser ExpireArgs
pExpireArgs =
  ExpireArgs
    <$> option
      auto
      ( long "threshold"
          <> short 't'
          <> help "Expiration threshold. Entries with a monthly energy below this will be removed."
          <> metavar "FLOAT"
          <> value 0.1
          <> showDefault
      )

readInput :: IO [NEString]
readInput = mapMaybe stripWhitespace . lines <$> getContents

augment :: AugmentArgs -> IO (Frecencies -> Frecencies)
augment (AugmentArgs False False) = pure id
augment (AugmentArgs aug res) = do
  strs <- readInput
  pure $ \(Frecencies t fs) -> Frecencies t $
    case (aug, res) of
      (False, True) -> Map.fromList [(str, nrg) | str <- strs, nrg <- toList (Map.lookup str fs)]
      (True, False) -> foldr (\str -> Map.insertWith (<>) str (Energy 0 0 0)) fs strs
      (True, True) -> Map.fromList $ (\key -> maybe (key, Energy 0 0 0) (key,) (Map.lookup key fs)) <$> strs
      _ -> fs

data AugmentArgs = AugmentArgs
  { _aaAugment :: Bool,
    _aaRestrict :: Bool
  }

pAugmentArgs :: Parser AugmentArgs
pAugmentArgs =
  AugmentArgs
    <$> flag False True (long "augment" <> short 'a' <> help "Augment the output with entries read from stdin, treating them as if they had a score of 0 if they are not present in the history")
    <*> flag False True (long "restrict" <> short 'r' <> help "Only output entries present in the keys read from stdin")

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

expire :: ExpireArgs -> Frecencies -> Frecencies
expire (ExpireArgs threshold) (Frecencies t fs) = Frecencies t (Map.filter ((> threshold) . monthly) fs)

-- TODO bump by 1 1 1, multiply scores before presenting
bump :: NEString -> Frecencies -> Frecencies
bump str (Frecencies t fs) = Frecencies t (Map.insertWith (<>) str (Energy 1 1 1) fs)

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
writeFrecencies path fs = do
  createDirectoryIfMissing True (takeDirectory path)
  atomicWriteFile path (encode fs)

printScores :: Weights -> Frecencies -> IO ()
printScores weights (Frecencies _ fs) = do
  printf "weighted score\thourly\t\tdaily\t\tmonthly\n"
  forM_ (sortOn (negate . score weights . snd) $ Map.toList fs) $ \(str, nrg@(Energy h d m)) ->
    printf "%12.6f\t%10.6f\t%10.6f\t%10.6f\t%s\n" (score weights nrg) h d m (unNEString str)
