{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Let
  ( Let(..)
  , letSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Let = Let T.Text
  deriving (Show, Eq)

letSchema :: FC.Fleece t => FC.Schema t Let
letSchema =
  FC.coerceSchema FC.text