{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.ReferenceResponses.Response500Body.Error
  ( Error(..)
  , errorSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Error = Error T.Text
  deriving (Show, Eq)

errorSchema :: FC.Fleece t => FC.Schema t Error
errorSchema =
  FC.coerceSchema FC.text