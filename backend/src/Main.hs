{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE MultiWayIf, KindSignatures, ViewPatterns #-}

import Focus.Backend (withFocus)
import Snap
import Reflex.Dom.Builder.Static

import Focus.Weblayouts.Backend.KissBackend
import Frontend.App --used for siteHead & siteBody
import Common.Route --used for urlToRoute function & Route data type


main :: IO ()
main = do 
  -- | get the ByteString of the statically rendered <head>
  theHead <- fmap snd $ renderStatic siteHead
  withFocus . quickHttpServe $ rootHandler theHead siteBody Route_Home
