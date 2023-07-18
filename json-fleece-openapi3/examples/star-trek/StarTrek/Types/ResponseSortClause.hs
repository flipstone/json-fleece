{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponseSortClause
  ( ResponseSortClause(..)
  , responseSortClauseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ResponseSortClause.ClauseOrder as ClauseOrder
import qualified StarTrek.Types.ResponseSortClause.Name as Name
import qualified StarTrek.Types.ResponseSortDirection as ResponseSortDirection

data ResponseSortClause = ResponseSortClause
  { clauseOrder :: ClauseOrder.ClauseOrder -- ^ Order in which this clause was applied
  , direction :: ResponseSortDirection.ResponseSortDirection -- ^ Sort direction
  , name :: Name.Name -- ^ Field name results are sorted by
  }
  deriving (Eq, Show)

responseSortClauseSchema :: FC.Fleece schema => schema ResponseSortClause
responseSortClauseSchema =
  FC.object $
    FC.constructor ResponseSortClause
      #+ FC.required "clauseOrder" clauseOrder ClauseOrder.clauseOrderSchema
      #+ FC.required "direction" direction ResponseSortDirection.responseSortDirectionSchema
      #+ FC.required "name" name Name.nameSchema