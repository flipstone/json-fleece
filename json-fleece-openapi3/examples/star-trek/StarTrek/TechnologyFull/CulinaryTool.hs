{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.CulinaryTool
  ( CulinaryTool(..)
  , culinaryToolSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CulinaryTool = CulinaryTool Bool
  deriving (Show, Eq)

culinaryToolSchema :: FC.Fleece schema => schema CulinaryTool
culinaryToolSchema =
  FC.coerceSchema FC.boolean