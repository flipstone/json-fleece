{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponseSortClause
  ( ResponseSortClause(..)
  , responseSortClauseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Show)
import StarTrek.ResponseSortDirection (ResponseSortDirection, responseSortDirectionSchema)

data ResponseSortClause = ResponseSortClause
  { name :: Text -- ^ Field name results are sorted by
  , clauseOrder :: Integer -- ^ Order in which this clause was applied
  , direction :: ResponseSortDirection -- ^ Sort direction
  }
  deriving (Eq, Show)

responseSortClauseSchema :: FC.Fleece schema => schema ResponseSortClause
responseSortClauseSchema =
  FC.object $
    FC.constructor ResponseSortClause
      #+ FC.required "name" name FC.text
      #+ FC.required "clauseOrder" clauseOrder FC.integer
      #+ FC.required "direction" direction responseSortDirectionSchema