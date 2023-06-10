{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Derivation
  ( Derivation(..)
  , Output(..)
  , Dependencies(..)
  , WhyDepends(..)
  , PathInfo(..)
  , pathInfoClosureSize
  , pathInfoDeriver
  , pathInfoNarSize
  , pathInfoPath
  , pathInfoReferences
  , depsGetDerivation
  , drvOutputPaths
  , readDerivation
  , readDependencies
  , readPathInfos
  , whyDepends
  ) where

import Control.Arrow ((&&&))
import Control.Lens hiding (mapOf)
import Data.Aeson qualified as Aeson
import Data.Functor (($>), void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import System.Process (readProcess)

import Nixplorer.Prelude

type Parser a = Parsec Void Text a

data Output = Output
  { outputPath      :: StorePath
  , outputAlgorithm :: Text
  , outputHash      :: Text
  }

-- The information contained in a derivation file, excluding its own name.
data Derivation = Derivation
  { drvOutputs     :: Map Text Output
  , drvInputs      :: Map StorePath [Text]
  , drvSources     :: [StorePath]
  , drvSystem      :: Text
  , drvBuilder     :: StorePath
  , drvArguments   :: [Text]
  , drvEnvironment :: Map Text Text
  }

drvOutputPaths :: Derivation -> [StorePath]
drvOutputPaths = map outputPath . Map.elems . drvOutputs

-- A root derivation and all of its recursive dependencies as well as reverse
-- links back to the root.
data Dependencies = Dependencies
  { depsOf          :: StorePath
  , depsStart       :: Derivation
  , depsDerivations :: Map StorePath Derivation
  , depsUses        :: Map StorePath (Set StorePath)
  }

-- Two derivations and the set of derivations that the first one depends on and
-- that depend on the second one. This can be combined with a Dependencies
-- record to display all paths through which the first derivation depends on the
-- second.
data WhyDepends = WhyDepends
  { whyDoes     :: StorePath
  , whyDependOn :: StorePath
  , whyReasons  :: Set StorePath
  }

-- | Info for an output path.
data PathInfo = PathInfo
  { _pathInfoPath        :: StorePath
  , _pathInfoDeriver     :: StorePath
  , _pathInfoNarSize     :: Int
  , _pathInfoClosureSize :: Int
  , _pathInfoReferences  :: Set StorePath
  } deriving (Generic)

makeLenses ''PathInfo

instance FromJSON PathInfo where
  parseJSON = parseJSONStripPrefix "_pathInfo"

readPathInfos :: (MonadFail m, MonadIO m) => StorePath -> m (Map StorePath PathInfo)
readPathInfos path =
  liftIO (readProcess "nix" ["path-info", "--recursive", "--json", path ^. storePathString] "")
  >>= decodeStringOrFail <&> Map.fromList . map (view pathInfoPath &&& id)

-- Dependencies

readDependencies :: StorePath -> IO Dependencies
readDependencies path = do
  depsStart <- readDerivation path
  depsDerivations <- go [path] mempty
  pure Dependencies
    { depsOf = path
    , depsStart
    , depsDerivations
    , depsUses = transposeMap $ Map.keysSet . drvInputs <$> depsDerivations
    }

  where
    go :: [StorePath] -> Map StorePath Derivation -> IO (Map StorePath Derivation)
    go [] done = pure done
    go (todo : todos) done
      | todo `Map.member` done = go todos done
      | otherwise = do
          drv <- readDerivation todo
          go (Map.keys (drvInputs drv) <> todos) (done & Map.insert todo drv)

-- | errors if derivation is not found
depsGetDerivation :: Dependencies -> StorePath -> Derivation
depsGetDerivation deps path = fromMaybe (error msg) $
  Map.lookup path $ depsDerivations deps
  where
    msg = "derivation not found: " <> path ^. storePathString

whyDepends :: Dependencies -> StorePath -> WhyDepends
whyDepends deps whyDependOn = WhyDepends
  { whyDoes = depsOf deps
  , whyDependOn
  , whyReasons = completeSet getUses $ Set.singleton whyDependOn
  }
  where
    getUses :: StorePath -> Set StorePath
    getUses path = Map.findWithDefault Set.empty path (depsUses deps)

completeSet :: Ord a => (a -> Set a) -> Set a -> Set a
completeSet f xs = go xs xs
  where
    go toExpand seen
      | Set.null toExpand' = seen
      | otherwise          = go toExpand' (Set.union toExpand' seen)
      where
        toExpand' = Set.unions (Set.map f toExpand) `Set.difference` seen

transposeMap :: (Ord k, Ord v) => Map k (Set v) -> Map v (Set k)
transposeMap = Map.fromListWith Set.union . concatMap getLinks . Map.toList
  where
    getLinks (key, values) = (, Set.singleton key) <$> Set.toList values

-- Parsing

readDerivation :: StorePath -> IO Derivation
readDerivation p = Text.readFile path
  >>= either (fail . errorBundlePretty) pure . parse derivation path
  where
    path = p ^. storePathString

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
storePath = review storePathText <$> text

text :: Parser Text
text = fmap Text.concat $ chunk "\"" *> many char <* chunk "\""

char :: Parser Text
char = chunk "\\\"" $> "\""
   <|> chunk "\\\\" $> "\\"
   <|> chunk "\\n" $> "\n"
   <|> chunk "\\t" $> "\t"
   <|> takeWhile1P Nothing (\c -> c /= '\\' && c /= '"')
