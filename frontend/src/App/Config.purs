module App.Config where

type Config =
  { title :: String
  , public_path :: String
  , api_path :: String
  }

foreign import config :: Config
