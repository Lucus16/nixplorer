{-# language QuasiQuotes #-}

module Nix.Db where

import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Control.Arrow (second)

import Nixplorer.Prelude

-- Note that the database is opened immutable although it isn't actually
-- immutable, it just isn't mutated often. This means it may sometimes throw an
-- exception.
withNixDb :: (Connection -> IO a) -> IO a
withNixDb = withConnection "file:/nix/var/nix/db/db.sqlite?immutable=1"

getNarSizes :: Connection -> IO (Map StorePath Int)
getNarSizes conn =
  Map.fromList . map (first (review storePathText))
  <$> query_ conn [sql|SELECT path, narSize FROM ValidPaths|]

getAllRefs :: Connection -> IO (Map StorePath (Set StorePath))
getAllRefs conn = Map.fromListWith Set.union . map (second Set.singleton) <$>
  query_ conn [sql|
    SELECT p1.path, p2.path
    FROM Refs
    LEFT JOIN ValidPaths AS p1 ON referrer = p1.id
    LEFT JOIN ValidPaths AS p2 ON reference = p2.id
  |]
