module Main (main) where

import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Nix.Derivation (readDerivation)
import Nixplorer.Prelude
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Lens

main :: IO ()
main = do
  drvs <- listDirectory "/nix/store"
    <&> map (review storePathString . ("/nix/store" </>)) . filter (".drv" `isSuffixOf`)
  traverse_ readDerivation drvs
