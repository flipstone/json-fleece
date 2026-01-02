{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.Name
  ( Name(..)
  , nameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Name = Name T.Text
  deriving (Show, Eq)

nameSchema :: FC.Fleece t => FC.Schema t Name
nameSchema =
  FC.coerceSchema FC.text