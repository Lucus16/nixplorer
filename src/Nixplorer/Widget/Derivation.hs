{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Widget.Derivation where

import Control.Lens
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)

import Brick qualified
import Brick ((<+>), (<=>), BrickEvent(..), hBox, padLeftRight, str, txt)
import Brick.Widgets.List (handleListEvent, list, renderList)

import Nix.Derivation (Derivation(..), readDerivation)
import Nixplorer.Prelude

data State = State
  { _statePath :: StorePath
  , _stateInputs :: List (StorePath, [Text])
  }

makeLenses ''State

new :: StorePath -> IO State
new path = do
  drv <- readDerivation path >>= either fail pure
  let inputs = Seq.fromList $ Map.toList $ drvInputs drv
  pure State
    { _statePath = path
    , _stateInputs = list (InputsFor path) inputs 0
    }

draw :: State -> Widget
draw state = str (state ^. statePath)
  <=> renderList renderInput True (state ^. stateInputs)
  where
    renderInput :: Bool -> (StorePath, [Text]) -> Widget
    renderInput focus (path, outputs) = focussedIf focus $
      padLeftRight 2 (str path)
      <+> hBox (map (padLeftRight 1 . txt) outputs)

handleEvent :: Event e -> Brick.EventM WidgetName State ()
handleEvent (VtyEvent ev) = Brick.zoom stateInputs $ handleListEvent ev
handleEvent _ = pure ()
