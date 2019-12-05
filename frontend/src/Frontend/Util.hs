{-# LANGUAGE OverloadedStrings #-}

module Frontend.Util where

import qualified Data.Text as T
import Reflex.Dom

card :: DomBuilder t m => m a -> m a
card w = divClass "card" $ divClass "card-body" w

code :: DomBuilder t m => [T.Text] -> m ()
code l = el "pre" $ el "code" $ text $ T.unlines l

inlineCode :: DomBuilder t m => T.Text -> m ()
inlineCode t = el "code" $ text t

key :: DomBuilder t m => T.Text -> T.Text -> m ()
key k v = el "div" $ do
  el "var" $ text k
  el "p" $ text v
