{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend (frontend) where

import Common.Route
import Frontend.CommonWidgets
import Frontend.Head
import Frontend.Footer
import Frontend.Nav
import Frontend.Page.Home
import Frontend.Page.GetStarted
import Frontend.Page.Resources
import Frontend.Page.Tutorial
import Frontend.Example

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad(forM_)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Frontend.Page.Examples.Todo as Todo
import qualified Examples.Todo as Todo

import qualified Frontend.Page.Examples.CollapsingContent as CollapsingContent
import qualified Examples.CollapsingContent as CollapsingContent

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = minWidth "1140px" $ do
      el "header" nav
      subRoute_ $ \case
        Route_Home -> home
        Route_GetStarted -> sectionPage (Route_GetStarted :/ ()) getStarted
        Route_Tutorial -> do
          el "main" $ el "article" $ tutorial
          enablePrismJS
        Route_Resources -> sectionPage (Route_Resources :/ ()) resources
        Route_Examples -> do
          subRoute_ $ \case
            ExamplesRoute_LandingPage -> landingPage
            ExamplesRoute_Todo -> formatExample Todo.article Todo.app
            ExamplesRoute_CollapsingContent -> formatExample CollapsingContent.article CollapsingContent.main
      el "footer" footer
  }

minWidth :: DomBuilder t m => Text -> m a -> m a
minWidth txt = elAttr "div" (Map.singleton "style" $ "min-width: " <> txt)

landingPage :: (DomBuilder t m, SetRoute t (R Route) m, RouteToUrl (R Route) m, Prerender js0 t m) => m ()
landingPage = el "main" $ el "article" $ do
  el "h1" $ text "Examples"
  elClass "p" "description" $ text "These examples are self-contained sample UIs intended to illustrate with real code how to build commonly used web functionalities with Reflex"
  el "hr" blank
  el "article" $ do
    forM_ [Todo.article, CollapsingContent.article] $ \article -> do
       routeLinkScrollToTop (Route_Examples :/ _example_route article) $ text $ _example_title article
       elClass "p" "description" $ text $ _example_description article
