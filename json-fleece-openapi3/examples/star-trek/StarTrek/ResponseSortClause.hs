{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponseSortClause
  ( ResponseSortClause(..)
  , responseSortClauseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ResponseSortClause.ClauseOrder (ClauseOrder, clauseOrderSchema)
import StarTrek.ResponseSortClause.Name (Name, nameSchema)
import StarTrek.ResponseSortDirection (ResponseSortDirection, responseSortDirectionSchema)

data ResponseSortClause = ResponseSortClause
  { name :: Name -- ^ Field name results are sorted by
  , clauseOrder :: ClauseOrder -- ^ Order in which this clause was applied
  , direction :: ResponseSortDirection -- ^ Sort direction
  }
  deriving (Eq, Show)

responseSortClauseSchema :: FC.Fleece schema => schema ResponseSortClause
responseSortClauseSchema =
  FC.object $
    FC.constructor ResponseSortClause
      #+ FC.required "name" name nameSchema
      #+ FC.required "clauseOrder" clauseOrder clauseOrderSchema
      #+ FC.required "direction" direction responseSortDirectionSchema