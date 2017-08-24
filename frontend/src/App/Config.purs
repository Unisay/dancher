module App.Config where

type Config =
  { title      :: String
  , pathPublic :: String
  , pathApi    :: String
  , fbAppId    :: String
  }

foreign import config :: Config
