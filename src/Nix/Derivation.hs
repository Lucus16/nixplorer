{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Derivation where

import Data.Either.Extra (mapLeft)
import Data.Functor ((<&>), ($>), void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Text.Megaparsec

import Nixplorer.Prelude

type Parser a = Parsec Void Text a

data Output = Output
  { outputPath :: StorePath
  , outputAlgorithm :: Text
  , outputHash :: Text
  }

data Derivation = Derivation
  { drvOutputs :: Map Text Output
  , drvInputs :: Map StorePath [Text]
  , drvSources :: [StorePath]
  , drvSystem :: Text
  , drvBuilder :: StorePath
  , drvArguments :: [Text]
  , drvEnvironment :: Map Text Text
  }

-- Parsing

readDerivation :: StorePath -> IO (Either String Derivation)
readDerivation (StorePath p) =
  Text.readFile path <&> mapLeft errorBundlePretty . parse derivation path
  where
    path = Text.unpack p

derivation :: Parser Derivation
derivation = do
  void $ chunk "Derive("
  drv <- Derivation
    <$> mapOf text output <* chunk ","
    <*> mapOf storePath (listOf text) <* chunk ","
    <*> listOf storePath <* chunk ","
    <*> text <* chunk ","
    <*> storePath <* chunk ","
    <*> listOf text <* chunk ","
    <*> mapOf text text
  void $ chunk ")"
  eof
  pure drv

output :: Parser Output
output = Output <$> storePath <* chunk "," <*> text <* chunk "," <*> text

mapOf :: Ord k => Parser k -> Parser v -> Parser (Map k v)
mapOf parseKey parseValue = Map.fromList <$> listOf do
  void $ chunk "("
  key <- parseKey
  void $ chunk ","
  value <- parseValue
  void $ chunk ")"
  pure (key, value)

listOf :: Parser a -> Parser [a]
listOf p = chunk "[" *> sepBy p (chunk ",") <* chunk "]"

storePath :: Parser StorePath
storePath = StorePath <$> text

text :: Parser Text
text = fmap Text.concat $ chunk "\"" *> many char <* chunk "\""

char :: Parser Text
char = chunk "\\\"" $> "\""
   <|> chunk "\\\\" $> "\\"
   <|> chunk "\\n" $> "\n"
   <|> chunk "\\t" $> "\t"
   <|> takeWhile1P Nothing (\c -> c /= '\\' && c /= '"')
