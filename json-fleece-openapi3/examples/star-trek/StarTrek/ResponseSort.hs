{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponseSort
  ( ResponseSort(..)
  , responseSortSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponseSortClause (ResponseSortClause, responseSortClauseSchema)

data ResponseSort = ResponseSort
  { clauses :: Maybe [ResponseSortClause] -- ^ List of response sort rules
  }
  deriving (Eq, Show)

responseSortSchema :: FC.Fleece schema => schema ResponseSort
responseSortSchema =
  FC.object $
    FC.constructor ResponseSort
      #+ FC.optionalField FC.OmitKey_DelegateNull "clauses" clauses (FC.list responseSortClauseSchema)