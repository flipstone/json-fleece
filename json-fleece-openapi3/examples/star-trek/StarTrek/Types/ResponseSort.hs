{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponseSort
  ( ResponseSort(..)
  , responseSortSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponseSortClause as ResponseSortClause

data ResponseSort = ResponseSort
  { clauses :: Maybe [ResponseSortClause.ResponseSortClause] -- ^ Single response sort clause
  }
  deriving (Eq, Show)

responseSortSchema :: FC.Fleece t => FC.Schema t ResponseSort
responseSortSchema =
  FC.object $
    FC.constructor ResponseSort
      #+ FC.optional "clauses" clauses (FC.list ResponseSortClause.responseSortClauseSchema)