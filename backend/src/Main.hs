{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE MultiWayIf, KindSignatures, ViewPatterns #-}

import Snap
import Reflex.Dom.Builder.Static

import Obelisk.Example.Backend.KissBackend
import Frontend.App --used for siteHead & siteBody
import Common.Route --used for urlToRoute function & Route data type


main :: IO ()
main = do 
  -- | get the ByteString of the statically rendered <head>
  theHead <- fmap snd $ renderStatic siteHead
  quickHttpServe $ rootHandler theHead siteBody Route_Home
