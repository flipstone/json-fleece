{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponseSort
  ( ResponseSort(..)
  , responseSortSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponseSortClause (ResponseSortClause, responseSortClauseSchema)

data ResponseSort = ResponseSort
  { clauses :: Maybe [ResponseSortClause] -- ^ Single response sort clause
  }
  deriving (Eq, Show)

responseSortSchema :: FC.Fleece schema => schema ResponseSort
responseSortSchema =
  FC.object $
    FC.constructor ResponseSort
      #+ FC.optional "clauses" clauses (FC.list responseSortClauseSchema)