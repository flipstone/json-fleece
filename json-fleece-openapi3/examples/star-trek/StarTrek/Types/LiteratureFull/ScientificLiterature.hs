{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureFull.ScientificLiterature
  ( ScientificLiterature(..)
  , scientificLiteratureSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ScientificLiterature = ScientificLiterature Bool
  deriving (Show, Eq)

scientificLiteratureSchema :: FC.Fleece schema => schema ScientificLiterature
scientificLiteratureSchema =
  FC.coerceSchema FC.boolean