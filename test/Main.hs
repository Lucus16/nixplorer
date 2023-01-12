module Main (main) where

import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Nix.Derivation (readDerivation)
import System.Directory (listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  drvs <- listDirectory "/nix/store" <&> map ("/nix/store" </>) . filter (".drv" `isSuffixOf`)
  traverse_ (readDerivation >=> either fail pure) drvs
