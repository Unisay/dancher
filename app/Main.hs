module Main where

import Lib
import Protolude
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 application
