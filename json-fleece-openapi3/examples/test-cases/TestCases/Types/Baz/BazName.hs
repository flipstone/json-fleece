{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Baz.BazName
  ( BazName(..)
  , bazNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype BazName = BazName T.Text
  deriving (Show, Eq)

bazNameSchema :: FC.Fleece t => FC.Schema t BazName
bazNameSchema =
  FC.coerceSchema FC.text