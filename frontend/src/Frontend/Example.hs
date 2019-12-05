{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Frontend.Example where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import Frontend.CommonWidgets
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import System.IO (FilePath)
import qualified Text.MMark.Internal.Type as MMark

import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom
import Reflex.MMark.Render (renderReflex)

import Common.Route

data Example = Example
  { _example_title :: !Text
  , _example_route :: !(R ExamplesRoute)
  , _example_description :: !Text
  , _example_fileContent :: !Text
  }

formatExample ::
  forall js t m.
  ( RouteToUrl (R Route) m
  , DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Prerender js t m) => Example -> m () -> m ()
formatExample example resultingWidget = do
  sectionPage (Route_Examples :/ _example_route example) section
  enablePrismJS
  where
    section :: Section m
    section = Section
      { _section_title = "Example: " <> _example_title example
      , _section_content = do
          elClass "p" "description" $ text $ _example_description example
      , _section_subsections = [reflexCode, result]
      }

    reflexCode:: DomBuilder t m => Section m
    reflexCode = Section
      { _section_title = "Reflex Code"
      , _section_content = do
        renderReflex [MMark.CodeBlock (Just "haskell") (_example_fileContent example)]
      , _section_subsections = []
      }

    result :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Section m
    result = Section
      { _section_title = "Result"
      , _section_content = resultingWidget
      , _section_subsections = []
      }

enablePrismJS :: (Prerender js t m, DomBuilder t m) => m ()
enablePrismJS = do
  -- Prism is in prerender so that it doesn't muck with the DOM until hydration is finished.
  -- It's here rather than in the head such that it runs when switching to this page with JS.
  prerender_ blank $ elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/prism.js") blank

textFromFile :: FilePath -> Q Exp
textFromFile fp = do
  qAddDependentFile fp
  txt <- fmap decodeUtf8 $ runIO $ BS.readFile fp
  lift (txt :: Text)
