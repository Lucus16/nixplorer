{-# LANGUAGE TemplateHaskell #-}

module Nixplorer (main) where

import Control.Lens
import Data.Functor (void)
import System.Environment (getArgs)

import Brick qualified
import Brick (App(..), BrickEvent(..), halt)

import Graphics.Vty.Attributes qualified as Vty

import Nix.Derivation
import Nixplorer.Prelude
import Nixplorer.Config
import Nixplorer.Widget.Derivation qualified as DrvWidget

data State = State
  { _stateConfig :: Config
  , _stateContents :: [DrvWidget.State]
  }

makeLenses ''State

browse :: FilePath -> Maybe FilePath -> IO ()
browse path mbDependOn = do
  rootWidget <- DrvWidget.new $ path ^. re storePathString
  config <- loadConfig $ path ^. re storePathString
  let mbFilter =
        FilterByWhyDepends . whyDepends (config ^. cfgRootDeps) . review storePathString
          <$> mbDependOn
  let state = State
        { _stateConfig   = config & cfgFilter .~ mbFilter
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
      [ attr "focussed"       $ bg Vty.brightWhite . fg Vty.black
      , attr "irrelevant"     $ fg Vty.brightBlack
      , attr "cursor"         $ styled Vty.reverseVideo
      , attr "irrelevant-cursor" $ styled Vty.reverseVideo . fg Vty.brightBlack
      , attr "varname"        $ styled Vty.bold
      , attr "matching path"  $ styled Vty.bold . fg Vty.brightYellow
      , attr "interpretation" $ fg Vty.brightBlack
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
    handleEvent (VtyEvent (Ctrl 's')) = stateConfig . cfgShowSize %= not
    handleEvent (VtyEvent (Ctrl 'o')) = stateConfig . cfgOrder    %= next
    handleEvent ev = Brick.zoom stateContents $ DrvWidget.handleEventStack ev

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> browse arg Nothing
    [why, does, dependOn] | why `elem` ["why", "why-depend", "why-depends"] -> browse does (Just dependOn)
    _     -> fail "usage: nixplorer /nix/store/foo.drv"
