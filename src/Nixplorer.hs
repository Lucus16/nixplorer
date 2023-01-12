module Nixplorer (main) where

import Data.Functor (void)
import System.Environment (getArgs)

import Brick qualified
import Brick (App(..), BrickEvent(..), halt)

import Graphics.Vty.Attributes qualified as Vty

import Nixplorer.Prelude
import Nixplorer.Widget.Derivation qualified as DrvWidget

browse :: FilePath -> IO ()
browse path = do
  state <- DrvWidget.new path

  void $ Brick.defaultMain app [state]
  where
    app :: App [DrvWidget.State] e WidgetName
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

    draw = pure . DrvWidget.draw . head

    handleEvent :: Event e -> Brick.EventM WidgetName [DrvWidget.State] ()
    handleEvent (VtyEvent (Ctrl 'q')) = halt
    handleEvent (VtyEvent (Ctrl 'c')) = halt
    handleEvent ev = DrvWidget.handleEventStack ev

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> browse arg
    _     -> fail "usage: nixplorer /nix/store/foo.drv"
