{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Page.Examples.Todo where

import Obelisk.Route
import Common.Route
import Frontend.Example

article :: Example
article = Example
  { _example_title = "Basic To-do List"
  , _example_route = ExamplesRoute_Todo :/ ()
  , _example_description = "A basic to-do list example, demonstrating how to use an input, how easy it is to work with lists with Reflex - adding to lists, merging lists, and deleting items."
  , _example_fileContent = $(textFromFile "frontend/src/Examples/Todo.hs")
  }
