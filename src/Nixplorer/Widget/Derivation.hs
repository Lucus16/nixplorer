{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Widget.Derivation where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Sequence qualified as Seq
import Data.Text (Text)

import Brick qualified
import Brick ((<+>), (<=>), BrickEvent(..), hBox, padLeftRight, str, txt)
import Brick.Widgets.List (handleListEvent, list, listSelectedElement, renderList)

import Graphics.Vty.Input.Events qualified as Vty

import Nix.Derivation (Derivation(..), readDerivation)
import Nixplorer.Prelude

data State = State
  { _statePath :: StorePath
  , _stateInputs :: List (StorePath, [Text])
  }

newtype Action = EnterInput StorePath

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

enterInput :: Brick.EventM n State (Maybe Action)
enterInput =
  uses stateInputs listSelectedElement <&> fmap (EnterInput . fst . snd)

handleEvent :: Event e -> Brick.EventM WidgetName State (Maybe Action)
handleEvent (VtyEvent (Vty.EvKey Vty.KRight [])) = enterInput
handleEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = enterInput
handleEvent (VtyEvent (Ctrl ']'))                = enterInput
handleEvent (VtyEvent ev) = Brick.zoom stateInputs (handleListEvent ev) >> pure Nothing
handleEvent _ = pure Nothing

popStack :: Brick.EventM n [a] ()
popStack = Brick.modify \case
  (_:x:xs) -> (x:xs)
  xs       -> xs

handleEventStack :: Event e -> Brick.EventM WidgetName [State] ()
handleEventStack (VtyEvent (Ctrl 't')) = popStack
handleEventStack (VtyEvent (Vty.EvKey Vty.KLeft [])) = popStack

handleEventStack ev = do
  actions :: [Action] <- Brick.zoom (ix 0) $ maybeToList <$> handleEvent ev
  for_ actions \case
    EnterInput path -> do
      widget <- liftIO $ new path
      Brick.modify (widget:)