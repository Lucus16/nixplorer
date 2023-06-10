{-# LANGUAGE TemplateHaskell #-}

module Nixplorer (main) where

import Control.Lens
import Data.Functor (void)
import System.Environment (getArgs)

import Brick qualified
import Brick (App(..), BrickEvent(..), halt)

import Graphics.Vty.Attributes qualified as Vty

import Nixplorer.Prelude
import Nixplorer.Config
import Nixplorer.Widget.Derivation qualified as DrvWidget

data State = State
  { _stateConfig :: Config
  , _stateContents :: [DrvWidget.State]
  }

makeLenses ''State

browse :: FilePath -> IO ()
browse path = do
  rootWidget <- DrvWidget.new $ path ^. re storePathString
  config <- loadConfig $ path ^. re storePathString
  let state = State
        { _stateConfig   = config
        , _stateContents = [rootWidget]
        }

  void $ Brick.defaultMain app state
  where
    app :: App State e WidgetName
    app = App
      { appDraw         = draw
      , appChooseCursor = \_ _ -> Nothing
      , appHandleEvent  = handleEvent
      , appStartEvent   = pure ()
      , appAttrMap      = const attrMap
      }

    attrMap = Brick.attrMap Vty.defAttr
      [ attr "focussed"      $ bg Vty.brightWhite . fg Vty.black
      , attr "irrelevant"    $ fg Vty.brightBlack
      , attr "cursor"        $ styled Vty.reverseVideo
      , attr "varname"       $ styled Vty.bold
      , attr "matching path" $ styled Vty.bold . fg Vty.brightYellow
      ]
      where
        attr :: String -> (Vty.Attr -> Vty.Attr) -> (Brick.AttrName, Vty.Attr)
        attr name f = (Brick.attrName name, f Vty.currentAttr)

        bg = flip Vty.withBackColor
        fg = flip Vty.withForeColor
        styled = flip Vty.withStyle

    draw state = [DrvWidget.drawStack config contents]
      where
        config   = state ^. stateConfig
        contents = state ^. stateContents

    handleEvent :: Event e -> Brick.EventM WidgetName State ()
    handleEvent (VtyEvent (Char 'q')) = halt
    handleEvent (VtyEvent (Ctrl 'q')) = halt
    handleEvent (VtyEvent (Ctrl 'c')) = halt
    handleEvent (VtyEvent (Ctrl 'p')) = stateConfig . cfgShowHash %= not
    handleEvent (VtyEvent (Ctrl 'o')) = stateConfig . cfgOrder    %= next
    handleEvent ev = Brick.zoom stateContents $ DrvWidget.handleEventStack ev

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> browse arg
    _     -> fail "usage: nixplorer /nix/store/foo.drv"
