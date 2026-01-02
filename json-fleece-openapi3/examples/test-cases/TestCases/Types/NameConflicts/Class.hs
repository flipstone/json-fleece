{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Class
  ( Class(..)
  , classSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Class = Class T.Text
  deriving (Show, Eq)

classSchema :: FC.Fleece t => FC.Schema t Class
classSchema =
  FC.coerceSchema FC.text