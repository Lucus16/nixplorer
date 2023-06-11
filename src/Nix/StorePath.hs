{-# LANGUAGE OverloadedStrings #-}

module Nix.StorePath where

import Control.Lens
import Control.Monad (replicateM)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec

import Nixplorer.Prelude

type PotentialStorePath = Text

findMatchingStorePaths :: [StorePath] -> Text -> [Either Text StorePath]
findMatchingStorePaths matchers = appendLefts . concatMap matchStorePath . findStorePaths
  where
    matchStorePath :: Either Text Text -> [Either Text StorePath]
    matchStorePath (Left x)                = [Left x]
    matchStorePath (Right pathWithGarbage) =
      case findPrefix (map (view storePathText) matchers) pathWithGarbage of
        Just (path, garbage) -> [Right (review storePathText path), Left garbage]
        Nothing              -> [Left pathWithGarbage]

    findPrefix :: [Text] -> Text -> Maybe (Text, Text)
    findPrefix [] _ = Nothing
    findPrefix (p : ps) t = case Text.stripPrefix p t of
      Just garbage -> Just (p, garbage)
      Nothing -> findPrefix ps t

findStorePaths :: Text -> [Either Text PotentialStorePath]
findStorePaths t = fromMaybe [Left t] $ parseMaybe (textWithStorePaths <* eof) t

textWithStorePaths :: Parser [Either Text PotentialStorePath]
textWithStorePaths = fmap appendLefts $ many $
  Right <$> try storePathWithGarbage
  <|> Left . Text.singleton <$> anySingle

appendLefts :: Monoid s => [Either s a] -> [Either s a]
appendLefts [] = []
appendLefts (Left x : Left y : xs) = appendLefts (Left (x <> y) : xs)
appendLefts (x : xs) = x : appendLefts xs

hashChars :: String
hashChars = "0123456789abcdfghijklmnpqrsvwxyz"

storePathWithGarbage :: Parser PotentialStorePath
storePathWithGarbage =
  chunk "/nix/store/" <> storeHash <> chunk "-"
  <> takeWhile1P Nothing (`notElem` ("/" :: String))

commonStorePathChars :: String
commonStorePathChars = "-_./" <> ['0'..'9'] <> ['A'..'Z'] <> ['a'..'z']

storeHash :: Parser Text
storeHash = Text.pack <$> replicateM 32 (satisfy (`elem` hashChars))

-- Store path using only common store path characters.
normalStorePath :: Parser Text
normalStorePath =
  chunk "/nix/store/" <> storeHash <> chunk "-"
  <> takeWhile1P Nothing (`elem` commonStorePathChars)
