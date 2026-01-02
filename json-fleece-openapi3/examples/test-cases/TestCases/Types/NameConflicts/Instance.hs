{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts.Instance
  ( Instance(..)
  , instanceSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Instance = Instance T.Text
  deriving (Show, Eq)

instanceSchema :: FC.Fleece t => FC.Schema t Instance
instanceSchema =
  FC.coerceSchema FC.text