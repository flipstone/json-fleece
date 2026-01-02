{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Baz.Bax
  ( Bax(..)
  , baxSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Bax = Bax T.Text
  deriving (Show, Eq)

baxSchema :: FC.Fleece t => FC.Schema t Bax
baxSchema =
  FC.coerceSchema FC.text