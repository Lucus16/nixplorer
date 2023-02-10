{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Config where

import Control.Lens

import Nixplorer.Prelude
import Nix.Derivation

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
  , _cfgOrder    :: Order
  , _cfgShowHash :: Bool
  , _cfgFilter   :: Maybe Filter
  }

makeLenses ''Config

loadConfig :: StorePath -> IO Config
loadConfig root = pure Config
  { _cfgRoot     = root
  , _cfgOrder    = OrderByName
  , _cfgShowHash = True
  , _cfgFilter   = Nothing
  }
