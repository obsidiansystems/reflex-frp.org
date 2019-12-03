{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
module Frontend.Markdown where

import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import Text.MMark ()

import System.IO (FilePath)

textFromFile :: FilePath -> Q Exp
textFromFile fp = do
  qAddDependentFile fp
  txt <- fmap decodeUtf8 $ runIO $ BS.readFile fp
  lift (txt :: Text)
