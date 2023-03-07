{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictBaseResponse
  ( ConflictBaseResponse(..)
  , conflictBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ConflictBase as ConflictBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ConflictBaseResponse = ConflictBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , conflicts :: Maybe [ConflictBase.ConflictBase] -- ^ Base conflict, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

conflictBaseResponseSchema :: FC.Fleece schema => schema ConflictBaseResponse
conflictBaseResponseSchema =
  FC.object $
    FC.constructor ConflictBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "conflicts" conflicts (FC.list ConflictBase.conflictBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema