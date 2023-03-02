{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponseSortClause.ClauseOrder
  ( ClauseOrder(..)
  , clauseOrderSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ClauseOrder = ClauseOrder Integer
  deriving (Show, Eq)

clauseOrderSchema :: FC.Fleece schema => schema ClauseOrder
clauseOrderSchema =
  FC.coerceSchema FC.integer