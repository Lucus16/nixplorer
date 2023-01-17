{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Prelude where

import Control.Lens
import Data.Function (on)
import Data.Tuple (swap)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lens (unpacked)

import Graphics.Vty.Input.Events qualified as Vty

import Brick qualified
import Brick.Widgets.List (GenericList)

newtype StorePath = StorePath { _unStorePath :: Text }
  deriving stock (Eq)
  deriving newtype (Show)

makeLenses ''StorePath

instance Ord StorePath where
  compare = compare `on` swap . Text.splitAt 44 . _unStorePath

data WidgetName
  = InputsFor StorePath
  | OutputsFor StorePath StorePath
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget WidgetName
type Event e = Brick.BrickEvent WidgetName e
type List e = GenericList WidgetName Seq e

storePathText :: Iso' StorePath Text
storePathText = iso _unStorePath StorePath

storePathString :: Iso' StorePath String
storePathString = storePathText . unpacked

pattern Ctrl :: Char -> Vty.Event
pattern Ctrl c = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]

pattern Char :: Char -> Vty.Event
pattern Char c = Vty.EvKey (Vty.KChar c) []

focussed :: Widget -> Widget
focussed = Brick.withAttr $ Brick.attrName "focussed"

focussedIf :: Bool -> Widget -> Widget
focussedIf True = focussed
focussedIf False = id
