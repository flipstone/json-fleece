{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Do
  ( Do(..)
  , doSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Do = Do T.Text
  deriving (Show, Eq)

doSchema :: FC.Fleece schema => schema Do
doSchema =
  FC.coerceSchema FC.text