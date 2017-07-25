{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE MultiWayIf, KindSignatures, ViewPatterns #-}

import Focus.Backend (withFocus)
import Focus.Backend.Snap (serveStaticIndex, appConfig_initialHead, appConfig_initialBody, serveApp)
import Focus.Weblayouts.Kiss
import Snap
import Focus.HTTP.Serve (serveAssets)
import Reflex.Dom.Builder.Static

import Data.Default
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text.Encoding
import Control.Lens
import Control.Monad.IO.Class

import Frontend.App --used for siteHead & siteBody
import Common.Route --used for urlToRoute function & Route data type

main :: IO ()
main = do 
  -- | get the ByteString of the statically rendered <head>
  theHead <- fmap snd $ renderStatic siteHead
  -- | 
  withFocus . quickHttpServe $ rootHandler theHead 

-- | Generates a MonadSnap object
rootHandler :: ByteString -> Snap ()
rootHandler theHead = route 
  [ ("", assetHandler) -- ^ (empty ByteString, MonadSnap holding static assets)
  , ("", do r <- getRequest
            let mRoute = urlToRoute $ decodeUtf8 $ rqURI r
            case mRoute of
              Nothing -> pass -- ^ Don't handle given request
              -- | handle request with updated content in the body
              Just rt -> serveStaticIndex $ def 
                & appConfig_initialHead .~ Just theHead
                & appConfig_initialBody .~ Just (liftIO $ fmap snd $ renderStatic $ siteBody rt)
            )
  , ("", appHandler theHead $ fmap snd $ renderStatic $ siteBody $ Route_Home) 
  -- ^ when the context path is an empty string, direct to Home
  ]

assetHandler :: Snap ()
assetHandler = do
  -- | get context path from MonadSnap object
  p <- getsRequest rqPathInfo
  -- | if context path ends with ".html" or doesn't contain a '.' ...
  if ".html" `BS.isSuffixOf` p || BSC.all (/= '.') p
    then serveAssets "assets" "static"
    -- ^ ...Serve static HTML files without redirecting
    else serveAssets "assets" "static"

-- | assign initialHead and initalBody to MonadSnap object
appHandler :: ByteString -> IO ByteString -> Snap ()
appHandler theHead theBody = serveApp "" $ def
  & appConfig_initialHead .~ Just theHead
  & appConfig_initialBody .~ Just (liftIO theBody)
