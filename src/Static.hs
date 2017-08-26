{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Static (StaticApi, staticApi, staticServer) where

import Servant
import Lib.Prelude
import Servant.Utils.StaticFiles (serveDirectoryWith)
import Network.Wai.Application.Static (defaultWebAppSettings, ssLookupFile)
import System.FilePath (addTrailingPathSeparator)
import WaiAppStatic.Types (StaticSettings, Pieces, unsafeToPiece, LookupResult(..))

type StaticApi = Raw

staticApi :: Proxy StaticApi
staticApi = Proxy

fileOrIndex :: StaticSettings -> Pieces -> IO LookupResult
fileOrIndex settings pieces = do
  res <- ssLookupFile settings pieces
  case res of
    LRNotFound -> ssLookupFile settings [unsafeToPiece "index.html"]
    _ -> return res

serveWebApp :: FilePath -> Server Raw
serveWebApp root =
  let settings = (defaultWebAppSettings . addTrailingPathSeparator) root
      customSettings = settings { ssLookupFile = fileOrIndex settings }
  in serveDirectoryWith customSettings

staticServer :: Server StaticApi
staticServer = serveWebApp "frontend/static"
