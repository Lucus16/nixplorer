module Nixplorer (main) where

import Brick

ui :: Widget ()
ui = str "hello"

main :: IO ()
main = simpleMain ui
