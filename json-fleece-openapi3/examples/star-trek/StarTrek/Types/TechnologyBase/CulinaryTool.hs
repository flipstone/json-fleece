{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.CulinaryTool
  ( CulinaryTool(..)
  , culinaryToolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CulinaryTool = CulinaryTool Bool
  deriving (Show, Eq)

culinaryToolSchema :: FC.Fleece t => FC.Schema t CulinaryTool
culinaryToolSchema =
  FC.coerceSchema FC.boolean