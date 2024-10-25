{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Nixplorer.Prelude
  ( StorePath, storePathText, storePathString, storePathName
  , Size
  , Widget, WidgetName(..), List, Event, EventM
  , Parser
  , pattern Ctrl, pattern Char
  , withAttrIf
  , fromMaybe
  , parseJSONStripPrefix
  , FromJSON, FromJSONKey
  , Generic
  , MonadIO, liftIO
  , decodeStringOrFail
  , tshow
  , toList
  , unless, when
  , (<&>)
  , first
  ) where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, FromJSONKey)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy.UTF8 qualified as BSL
import Data.Char (isUpper, toLower)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (stripPrefix)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lens (unpacked)
import Data.Tuple (swap)
import Data.Void (Void)
import GHC.Generics (Generic, Rep)
import Text.Megaparsec (Parsec)
import Database.SQLite.Simple.FromField (FromField(..))

import Graphics.Vty.Input.Events qualified as Vty

import Brick qualified
import Brick.Widgets.List (GenericList)

type Size = Int

newtype StorePath = StorePath { _unStorePath :: Text }
  deriving stock (Eq)
  deriving newtype (FromJSON, FromJSONKey, Show)

instance FromField StorePath where
  fromField field = review storePathText <$> fromField field

instance Ord StorePath where
  compare = compare `on` swap . Text.splitAt 44 . _unStorePath

data WidgetName
  = InputsFor StorePath
  | OutputsFor StorePath StorePath
  | ViewportFor StorePath
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget WidgetName
type Event e = Brick.BrickEvent WidgetName e
type List e = GenericList WidgetName Seq e
type EventM state a = Brick.EventM WidgetName state a
type Parser a = Parsec Void Text a

storePathText :: Iso' StorePath Text
storePathText = iso _unStorePath StorePath

storePathString :: Iso' StorePath String
storePathString = storePathText . unpacked

storePathName :: Getter StorePath Text
storePathName = storePathText . to (Text.drop 44)

pattern Ctrl :: Char -> Vty.Event
pattern Ctrl c = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]

pattern Char :: Char -> Vty.Event
pattern Char c = Vty.EvKey (Vty.KChar c) []

withAttrIf :: Bool -> String -> Widget -> Widget
withAttrIf False _ = id
withAttrIf True a = Brick.withAttr (Brick.attrName a)

parseJSONStripPrefix :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => String -> Aeson.Value -> Aeson.Parser a
parseJSONStripPrefix prefix = Aeson.genericParseJSON Aeson.defaultOptions
  { Aeson.fieldLabelModifier = modifier }
  where
    modifier name = case stripPrefix prefix name of
      Just (n : ame') | isUpper n -> toLower n : ame'
      _                           -> error $ "field " <> name <> " does not have prefix " <> prefix

decodeStringOrFail :: (FromJSON a, MonadFail m) => String -> m a
decodeStringOrFail = either fail pure . Aeson.eitherDecode . BSL.fromString

tshow :: Show a => a -> Text
tshow = Text.pack . show
