{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Widget.Derivation where

import Control.Lens
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text

import Brick qualified
import Brick ((<=>), BrickEvent(..), Padding(..), padLeft, txt)
import Brick.Widgets.List (handleListEvent, list, listSelectedElement, renderList)
import Graphics.Vty.Input.Events qualified as Vty
import System.Clipboard (setClipboardString)

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
draw state = txt (state ^. statePath . storePathText)
  <=> txt "input derivations:"
  <=> padLeft (Pad 2) (renderList renderInput True (state ^. stateInputs))
  where
    renderInput :: Bool -> (StorePath, [Text]) -> Widget
    renderInput focus (path, outputs) = focussedIf focus $ txt $
      path ^. storePathText <> " (" <> Text.intercalate ", " outputs <> ")"

forSelectedInput :: (StorePath -> [Text] -> Brick.EventM n State a) -> Brick.EventM n State (Maybe a)
forSelectedInput f = do
  uses stateInputs listSelectedElement >>= traverse (uncurry f . snd)

enterInput :: Brick.EventM n State (Maybe Action)
enterInput = forSelectedInput \path _ -> pure (EnterInput path)

handleEvent :: Event e -> Brick.EventM WidgetName State (Maybe Action)
handleEvent (VtyEvent (Vty.EvKey Vty.KRight [])) = enterInput
handleEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = enterInput
handleEvent (VtyEvent (Ctrl ']'))                = enterInput
handleEvent (VtyEvent (Char 'y')) = join <$> forSelectedInput \path _ -> do
  liftIO $ setClipboardString $ path ^. storePathString
  pure Nothing
handleEvent (VtyEvent ev) = Brick.zoom stateInputs (handleListEvent ev) >> pure Nothing
handleEvent _ = pure Nothing

popStack :: Brick.EventM n [a] ()
popStack = Brick.modify \case
  (_:x:xs) -> (x:xs)
  xs       -> xs

drawStack :: [State] -> Widget
drawStack [] = error "drawStack: State must be nonempty"
drawStack (s:ss) = foldl drawLevel (draw s) ss
  where
    drawLevel :: Widget -> State -> Widget
    drawLevel inner state = txt (state ^. statePath . storePathText)
      <=> padLeft (Pad 2) inner

handleEventStack :: Event e -> Brick.EventM WidgetName [State] ()
handleEventStack (VtyEvent (Ctrl 't')) = popStack
handleEventStack (VtyEvent (Vty.EvKey Vty.KLeft [])) = popStack

handleEventStack ev = do
  actions :: [Action] <- Brick.zoom (ix 0) $ maybeToList <$> handleEvent ev
  for_ actions \case
    EnterInput path -> do
      widget <- liftIO $ new path
      Brick.modify (widget:)
