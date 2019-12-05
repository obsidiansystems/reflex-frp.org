{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Examples.CollapsingContent (main) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  key "rec" "Requires {-# LANGUAGE RecursiveDo #-}. Allows the consumption of binds before they've been bound."
  key "toggle" "Takes an initial value and an event, produces a dynamic wrapped boolean. The value flips each time the event fires."
  key "void" "Allows us to cleanly discard values we don't care about."
  key "widgetHold" "Takes an initial widget, and an event that wraps a widget. The widget is replaced every time the event fires. It produces a dynamic wrapping the return value of the widget."

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import Control.Monad (void)"
    , "import Control.Monad.Fix (MonadFix)"
    , "import qualified Data.Text as T"
    , "import Reflex.Dom"
    ]

main :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Collapsible Content"
  el "p" $ text "Content you want shown and hidden when a trigger element is clicked. Common for menu options or expanding detail text."
  keyTools
  imports
  card $ do
    el "h5" $ text "Example: Click to show content."
    collapsibleContent "Click Me" content
  card $ do
    el "h5" $ text "Code"
    code
      [ "usage :: (DomBuilder t m, MonadFix m, MonadHold t m) => m ()"
      , "usage = collapsibleContent \"Click Me\" $ el \"div\" $ text \"Some content.\""
      , ""
      , "collapsibleContent ::  (MonadHold t m, MonadFix m, DomBuilder t m) => T.Text -> m () -> m ()"
      , "collapsibleContent t w = do"
      , "  (b, _) <- el' \"div\" $ "
      , "    elAttr \"a\" (\"href\" =: \"javascript:void(0);\") $ text t"
      , "  let evB = domEvent Click b"
      , "  dyC <- toggle False evB"
      , "  void $ widgetHold blank (menu <$> updated dyC)"
      , "  where"
      , "    menu True = w"
      , "    menu False = blank"
      ]
  card $ do
    el "h5" $ text "Example: Click to show content with chevron."
    collapsibleContentWithChevron "Click Me" content
  card $ do
    el "h5" $ text "Code"
    code
      [ "usage :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => m ()"
      , "usage = collapsibleContentWithChevron \"Click Me\" $ el \"div\" $ text \"Some content.\""
      , ""
      , "collapsibleContentWithChevron ::  (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m) => T.Text -> m () -> m ()"
      , "collapsibleContentWithChevron t w = do"
      , "  rec "
      , "    (b, _) <- el' \"div\" $ "
      , "      elAttr \"a\" (\"href\" =: \"javascript:void(0);\") $ do"
      , "        text t"
      , "        dynText $ chevron <$> dyC"
      , "    let evB = domEvent Click b"
      , "    dyC <- toggle False evB"
      , "  void $ widgetHold blank (menu <$> updated dyC)"
      , "  where"
      , "    menu True = w"
      , "    menu False = blank"
      , "    chevron True = \" ⇧\""
      , "    chevron False = \" ⇩\""
      ]

content :: DomBuilder t m => m ()
content = do
  el "div" $ text "Some content."

collapsibleContent ::  (MonadHold t m, MonadFix m, DomBuilder t m) => T.Text -> m () -> m ()
collapsibleContent t w = do
  (b, _) <- el' "div" $ 
    elAttr "a" ("href" =: "javascript:void(0);") $ text t
  let evB = domEvent Click b
  dyC <- toggle False evB
  void $ widgetHold blank (menu <$> updated dyC)
  where
    menu True = w
    menu False = blank

collapsibleContentWithChevron ::  (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m) => T.Text -> m () -> m ()
collapsibleContentWithChevron t w = do
  rec 
    (b, _) <- el' "div" $ 
      elAttr "a" ("href" =: "javascript:void(0);") $ do
        text t
        dynText $ chevron <$> dyC
    let evB = domEvent Click b
    dyC <- toggle False evB
  void $ widgetHold blank (menu <$> updated dyC)
  where
    menu True = w
    menu False = blank
    chevron True = " ⇧"
    chevron False = " ⇩"
