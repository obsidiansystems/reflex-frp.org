{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Page.Examples.CollapsingContent where

import Obelisk.Route
import Common.Route
import Frontend.Example

article :: Example
article = Example
  { _example_title = "Collapsing Content"
  , _example_route = ExamplesRoute_CollapsingContent :/ ()
  , _example_description = "Content to show or hide when a trigger element is clicked. Commonly used for menu options or expanding detail text."
  , _example_fileContent = $(textFromFile "frontend/src/Examples/CollapsingContent.hs")
  }
