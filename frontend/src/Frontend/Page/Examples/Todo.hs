{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Page.Examples.Todo (todo) where

import Control.Monad.Fix (MonadFix)
import Reflex.Dom
import Frontend.CommonWidgets
import Frontend.Markdown
import Reflex.MMark.Render (renderReflex)
import qualified Text.MMark.Internal.Type as MMark

import Examples.Todo (app)

todo :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Section m
todo = Section
  { _section_title = "Example: Basic To-do List"
  , _section_content = do
    elClass "p" "description" $ text "A basic to-do list example, demonstrating how to use an input, how easy it is to work with lists with Reflex - adding to lists, merging lists, and deleting items."
  , _section_subsections = [reflexCode, result]
  }

reflexCode :: DomBuilder t m => Section m
reflexCode = Section
  { _section_title = "Reflex Code"
  , _section_content = do
    renderReflex [MMark.CodeBlock (Just "haskell") $(textFromFile "frontend/src/Examples/Todo.hs")]
  , _section_subsections = []
  }

result :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Section m
result = Section
  { _section_title = "Result"
  , _section_content = do
    app
  , _section_subsections = []
  }
