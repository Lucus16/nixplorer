{-# LANGUAGE PatternSynonyms #-}

module Nixplorer.Prelude where

import Data.Sequence (Seq)

import Graphics.Vty.Input.Events qualified as Vty

import Brick qualified
import Brick.Widgets.List (GenericList)

type StorePath = String

data WidgetName
  = InputsFor StorePath
  | OutputsFor StorePath StorePath
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget WidgetName
type Event e = Brick.BrickEvent WidgetName e
type List e = GenericList WidgetName Seq e

pattern Ctrl :: Char -> Vty.Event
pattern Ctrl c = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]

pattern Char :: Char -> Vty.Event
pattern Char c = Vty.EvKey (Vty.KChar c) []

focussed :: Widget -> Widget
focussed = Brick.withAttr $ Brick.attrName "focussed"

focussedIf :: Bool -> Widget -> Widget
focussedIf True = focussed
focussedIf False = id
