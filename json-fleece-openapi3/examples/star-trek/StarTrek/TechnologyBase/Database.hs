{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase.Database
  ( Database(..)
  , databaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Database = Database Bool
  deriving (Show, Eq)

databaseSchema :: FC.Fleece schema => schema Database
databaseSchema =
  FC.coerceSchema FC.boolean