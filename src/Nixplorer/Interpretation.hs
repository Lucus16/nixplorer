{-# LANGUAGE OverloadedStrings #-}

module Nixplorer.Interpretation where

import Control.Lens hiding (mapOf)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec
import Data.Text.Encoding qualified as Text

import Nix.StorePath (normalStorePath)
import Nixplorer.Prelude

-- Interpretation

data InterpretedText = InterpretedText
  { itOriginal :: Text
  , itInterpretation :: Interpretation
  }

data Interpretation
  = StorePathList Text [StorePath]
  | RawLines [Text]
  | Json Aeson.Value
  deriving (Show)

interpretText :: Text -> InterpretedText
interpretText t = InterpretedText t $ interpret t

interpret :: Text -> Interpretation
interpret t = fromMaybe (RawLines $ Text.lines t) $
  parseMaybe interpretedEnvVar t <|> jsonVar t

jsonVar :: Text -> Maybe Interpretation
jsonVar t = do
  json <- Aeson.decodeStrict (Text.encodeUtf8 t)
  unless (isMultilineJson json) $ fail "not definitely json"
  pure $ Json json
  where
    isMultilineJson :: Aeson.Value -> Bool
    isMultilineJson Aeson.Object{} = True
    isMultilineJson Aeson.Array{} = True
    isMultilineJson _ = False

interpretedEnvVar :: Parser Interpretation
interpretedEnvVar = storePathsSepBy " " <|> storePathsSepBy ":"

storePathsSepBy :: Text -> Parser Interpretation
storePathsSepBy sep =
  StorePathList sep . map (review storePathText)
  <$> normalStorePath `sepBy` chunk sep
