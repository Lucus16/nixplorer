{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Nixplorer (main) where

import Control.Lens
import Data.Sequence (Seq)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import System.Environment (getArgs)

import Brick qualified
import Brick ((<+>), (<=>), App(..), BrickEvent(..), hBox, halt, padLeftRight, str, txt)
import Brick.Widgets.List (GenericList, handleListEvent, list, renderList)

import Graphics.Vty.Attributes qualified as Vty
import Graphics.Vty.Input.Events qualified as Vty

import Nix.Derivation (Derivation(..), StorePath, readDerivation)

data WidgetName
  = InputsFor StorePath
  deriving (Eq, Ord, Show)

type Widget = Brick.Widget WidgetName
type Event e = Brick.BrickEvent WidgetName e

pattern Ctrl :: Char -> Vty.Event
pattern Ctrl c = Vty.EvKey (Vty.KChar c) [Vty.MCtrl]

type List e = GenericList WidgetName Seq e

data DerivationWidgetState = DerivationWidgetState
  { _drvwsPath :: StorePath
  , _drvwsInputs :: List (StorePath, [Text])
  }

makeLenses ''DerivationWidgetState

loadDerivation :: StorePath -> IO DerivationWidgetState
loadDerivation path = do
  drv <- readDerivation path >>= either fail pure
  let inputs = Seq.fromList $ Map.toList $ drvInputs drv
  pure DerivationWidgetState
    { _drvwsPath = path
    , _drvwsInputs = list (InputsFor path) inputs 0
    }

focussed :: Widget -> Widget
focussed = Brick.withAttr $ Brick.attrName "focussed"

focussedIf :: Bool -> Widget -> Widget
focussedIf True = focussed
focussedIf False = id

drawDerivationWidget :: DerivationWidgetState -> Widget
drawDerivationWidget state = str (state ^. drvwsPath)
  <=> renderList renderItem True (state ^. drvwsInputs)
  where
    renderItem :: Bool -> (StorePath, [Text]) -> Widget
    renderItem focus (path, outputs) = focussedIf focus $
      padLeftRight 2 (str path)
      <+> hBox (map (padLeftRight 1 . txt) outputs)

browse :: FilePath -> IO ()
browse path = do
  state <- loadDerivation path

  void $ Brick.defaultMain app state
  where
    app :: App DerivationWidgetState e WidgetName
    app = App
      { appDraw = draw
      , appChooseCursor = \_ _ -> Nothing
      , appHandleEvent = handleEvent
      , appStartEvent = pure ()
      , appAttrMap = const attrMap
      }

    attrMap = Brick.attrMap Vty.defAttr
      [ attr "focussed" $ bg Vty.brightBlack
      ]
      where
        attr :: String -> (Vty.Attr -> Vty.Attr) -> (Brick.AttrName, Vty.Attr)
        attr name f = (Brick.attrName name, f Vty.currentAttr)
        bg    = flip Vty.withBackColor

    draw = pure . drawDerivationWidget
    handleEvent :: Event e -> Brick.EventM WidgetName DerivationWidgetState ()
    handleEvent (VtyEvent (Ctrl 'q')) = halt
    handleEvent (VtyEvent (Ctrl 'c')) = halt
    handleEvent (VtyEvent ev) = Brick.zoom drvwsInputs $ handleListEvent ev
    handleEvent _ = pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> browse arg
    _     -> fail "usage: nixplorer /nix/store/foo.drv"
