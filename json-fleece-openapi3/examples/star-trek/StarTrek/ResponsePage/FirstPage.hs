{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.FirstPage
  ( FirstPage(..)
  , firstPageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FirstPage = FirstPage Bool
  deriving (Show, Eq)

firstPageSchema :: FC.Fleece schema => schema FirstPage
firstPageSchema =
  FC.coerceSchema FC.boolean