module Main where

import Lib
import Protolude
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn ("Server listens at http://localhost:8081" :: Text)
  run 8081 application
