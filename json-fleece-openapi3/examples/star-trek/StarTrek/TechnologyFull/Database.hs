{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.Database
  ( Database(..)
  , databaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Database = Database Bool
  deriving (Show, Eq)

databaseSchema :: FC.Fleece schema => schema Database
databaseSchema =
  FC.coerceSchema FC.boolean