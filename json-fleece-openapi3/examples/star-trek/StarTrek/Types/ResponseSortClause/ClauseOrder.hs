{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponseSortClause.ClauseOrder
  ( ClauseOrder(..)
  , clauseOrderSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ClauseOrder = ClauseOrder Integer
  deriving (Show, Eq)

clauseOrderSchema :: FC.Fleece t => FC.Schema t ClauseOrder
clauseOrderSchema =
  FC.coerceSchema FC.integer