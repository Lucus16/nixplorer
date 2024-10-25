{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Config where

import Data.Set qualified as Set
import Control.Lens
import Data.Map (Map)
import Data.Set (Set)

import Nixplorer.Prelude
import Nix.Derivation
import Nix.Db

newtype Filter
  = FilterByWhyDepends WhyDepends

data Order
  = OrderByName
  | OrderBySize
  | OrderByClosureSize
  deriving (Bounded, Enum, Eq)

next :: (Bounded a, Enum a, Eq a) => a -> a
next e | e == maxBound = minBound
       | otherwise     = succ e

data Config = Config
  { _cfgRoot     :: StorePath
  , _cfgRootDeps :: Dependencies
  , _cfgNarSizes :: Map StorePath Int
  , _cfgAllRefs  :: Map StorePath (Set StorePath)
  , _cfgOrder    :: Order
  , _cfgShowHash :: Bool
  , _cfgShowSize :: Bool
  , _cfgFilter   :: Maybe Filter
  }

makeLenses ''Config

filterHasPath :: Config -> StorePath -> Bool
filterHasPath cfg path = case cfg ^. cfgFilter of
  Nothing -> True
  Just (FilterByWhyDepends w) -> path `Set.member` whyReasons w

loadConfig :: StorePath -> IO Config
loadConfig root = do
  (narSizes, allRefs) <- withNixDb $ \conn -> do
    narSizes <- getNarSizes conn
    allRefs <- getAllRefs conn
    pure (narSizes, allRefs)
  deps <- readDependencies allRefs root
  pure Config
    { _cfgRoot     = root
    , _cfgRootDeps = deps
    , _cfgNarSizes = narSizes
    , _cfgAllRefs  = allRefs
    , _cfgOrder    = OrderByName
    , _cfgShowHash = True
    , _cfgShowSize = True
    , _cfgFilter   = Nothing
    }
