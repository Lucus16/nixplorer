{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nixplorer.Widget.Derivation where

import Control.Lens
import Control.Monad (join)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.Megaparsec ((<|>), chunk, parseMaybe, sepBy)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.Scientific qualified as Scientific

import Brick qualified
import Brick ((<=>), (<+>), BrickEvent(..), Padding(..), padLeft, txt)
import Brick.Widgets.List (handleListEvent, list, listSelectedElement, listSelectedElementL, renderList)
import Graphics.Vty.Input.Events qualified as Vty
import System.Clipboard (setClipboardString)

import Nix.Derivation (Derivation(..), depsGetDerivation, drvOutputPaths, readDerivation)
import Nix.StorePath (findMatchingStorePaths, normalStorePath)
import Nixplorer.Config
import Nixplorer.Prelude

data State = State
  { _statePath :: StorePath
  , _stateDrv :: Derivation
  , _stateInputs :: List (StorePath, [Text])
  , _stateEnvScroll :: Brick.ViewportScroll WidgetName
  }

newtype Action = EnterInput StorePath

makeLenses ''State

stateSelectedInput :: Traversal' State StorePath
stateSelectedInput = stateInputs . listSelectedElementL . _1

new :: StorePath -> IO State
new path = do
  drv <- readDerivation path
  let inputs = Seq.fromList $ Map.toList $ drvInputs drv
  pure State
    { _statePath = path
    , _stateDrv = drv
    , _stateInputs = list (InputsFor path) inputs 0
    , _stateEnvScroll = Brick.viewportScroll $ ViewportFor path
    }

drawStorePath :: Config -> StorePath -> Widget
drawStorePath cfg path
  | cfg ^. cfgShowHash = txt (path ^. storePathText)
  | otherwise          = txt (path ^. storePathName)

draw :: Config -> State -> Widget
draw cfg state = drawStorePath cfg (state ^. statePath)
  <=> txt "input derivations:"
  <=> renderList renderInput True (state ^. stateInputs)
  <=> Brick.padTop (Brick.Pad 1) (txt "environment:")
  <=> Brick.vLimitPercent 75 environmentWidget
  where
    environmentWidget :: Widget
    environmentWidget = Brick.padLeft (Brick.Pad 2)
      $ Brick.viewport (ViewportFor (state ^. statePath)) Brick.Vertical
      $ Brick.vBox $ map (renderEnvVar cfg state . fmap interpretEnvVar)
      $ Map.assocs $ drvEnvironment $ state ^. stateDrv

    renderInput :: Bool -> (StorePath, [Text]) -> Widget
    renderInput focus (path, outputs) = styled $ txt $ pathText <> outputsText
      where
        pathText | cfg ^. cfgShowHash = path ^. storePathText
                 | otherwise          = path ^. storePathName
        outputsText = " (" <> Text.intercalate ", " outputs <> ")"
        styled = focussedIf focus

forSelectedInput :: (StorePath -> [Text] -> Brick.EventM n State a) -> Brick.EventM n State (Maybe a)
forSelectedInput f = do
  uses stateInputs listSelectedElement >>= traverse (uncurry f . snd)

enterInput :: Brick.EventM n State (Maybe Action)
enterInput = forSelectedInput \path _ -> pure (EnterInput path)

scrollEnv :: Brick.Direction -> EventM State ()
scrollEnv dir = do
  scrollState <- use stateEnvScroll
  Brick.vScrollPage scrollState dir

handleEvent :: Event e -> Brick.EventM WidgetName State (Maybe Action)
handleEvent (VtyEvent (Vty.EvKey Vty.KRight [])) = enterInput
handleEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = enterInput
handleEvent (VtyEvent (Ctrl ']'))                = enterInput
handleEvent (VtyEvent (Char 'y')) = join <$> forSelectedInput \path _ -> do
  liftIO $ setClipboardString $ path ^. storePathString
  pure Nothing
handleEvent (VtyEvent (Vty.EvKey Vty.KPageUp [])) = scrollEnv Brick.Up >> pure Nothing
handleEvent (VtyEvent (Vty.EvKey Vty.KPageDown [])) = scrollEnv Brick.Down >> pure Nothing
handleEvent (VtyEvent ev) = Brick.zoom stateInputs (handleListEvent ev) >> pure Nothing
handleEvent _ = pure Nothing

popStack :: Brick.EventM n [a] ()
popStack = Brick.modify \case
  (_:x:xs) -> (x:xs)
  xs       -> xs

drawStack :: Config -> [State] -> Widget
drawStack _ [] = error "drawStack: State must be nonempty"
drawStack cfg (s:ss) = foldl drawLevel (draw cfg s) ss
  where
    drawLevel :: Widget -> State -> Widget
    drawLevel inner state = drawStorePath cfg (state ^. statePath)
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

data InterpretedEnvVar
  = StorePathList Text [StorePath]
  | RawLines [Text]
  | Json Aeson.Value
  deriving (Show)

interpretEnvVar :: Text -> InterpretedEnvVar
interpretEnvVar t = fromMaybe (RawLines $ Text.lines t) $
  parseMaybe interpretedEnvVar t <|> jsonVar t

jsonVar :: Text -> Maybe InterpretedEnvVar
jsonVar t = do
  json <- Aeson.decodeStrict (Text.encodeUtf8 t)
  unless (isMultilineJson json) $ fail "not definitely json"
  pure $ Json json

interpretedEnvVar :: Parser InterpretedEnvVar
interpretedEnvVar = storePathsSepBy " " <|> storePathsSepBy ":"

storePathsSepBy :: Text -> Parser InterpretedEnvVar
storePathsSepBy sep =
  StorePathList sep . map (review storePathText)
  <$> normalStorePath `sepBy` chunk sep

renderEnvVar :: Config -> State -> (Text, InterpretedEnvVar) -> Widget
renderEnvVar cfg state (k, v) =
  case v of
    StorePathList _ [] -> interp "empty" Brick.emptyWidget
    StorePathList _ [path] -> single $ renderEnvLine (path ^. storePathText)
    StorePathList " " paths -> interp "list of store paths" $
      Brick.vBox (map (renderEnvLine . view storePathText) paths)
    StorePathList sep paths -> interp "list of store paths" $
      Brick.vBox (map ((txt sep <+>) . renderEnvLine . view storePathText) paths)
    Json json -> interp "json" $ renderJson json
    RawLines [line] -> single $ renderEnvLine line
    RawLines ls -> multi $ Brick.vBox (map renderEnvLine ls)
  where
    keyWidget = Brick.withAttr (Brick.attrName "varname") (txt k) <+> txt ":"
    single s = keyWidget <+> Brick.padLeft (Pad 1) s
    multi = interp mempty
    interp s m =
      keyWidget <+> Brick.padLeft (Pad 1) (Brick.withAttr (Brick.attrName "interpretation") (txt s))
      <=> Brick.padLeft (Pad 2) m

    highlightPaths :: [StorePath]
    highlightPaths = case state ^? stateSelectedInput of
      Nothing -> []
      Just selectedInput -> drvOutputPaths $
        depsGetDerivation (cfg ^. cfgRootDeps) selectedInput

    renderEnvLine :: Text -> Widget
    renderEnvLine =
      Brick.hBox
      . map (either txt highlightPath)
      . findMatchingStorePaths highlightPaths

    highlightPath :: StorePath -> Widget
    highlightPath = Brick.withAttr (Brick.attrName "matching path") . txt . view storePathText

    renderJson :: Aeson.Value -> Widget
    renderJson Aeson.Null = txt "null"
    renderJson (Aeson.Bool False) = txt "false"
    renderJson (Aeson.Bool True) = txt "true"
    renderJson (Aeson.Number n) = case Scientific.toBoundedInteger @Int n of
      Nothing -> txt $ tshow n
      Just i -> txt $ tshow i
    renderJson (Aeson.String t) = Brick.vBox $ map renderEnvLine $ Text.lines t
    renderJson (Aeson.Array xs) = Brick.vBox $ flip map (toList xs) \value ->
      Brick.txt "- " <+> renderJson value
    renderJson (Aeson.Object kvs) = Brick.vBox $ flip map (KeyMap.toList kvs) \(key, value) ->
      case value of
        Aeson.Array{} -> (renderKey key <+> txt ":") <=> renderJson value
        Aeson.Object{} -> (renderKey key <+> txt ":") <=> (txt "  " <+> renderJson value)
        _ -> renderKey key <+> txt ": " <+> renderJson value

    renderKey :: Aeson.Key -> Widget
    renderKey key = Brick.txt (Key.toText key)

isMultilineJson :: Aeson.Value -> Bool
isMultilineJson Aeson.Object{} = True
isMultilineJson Aeson.Array{} = True
isMultilineJson _ = False
