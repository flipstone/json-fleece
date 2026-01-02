{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureFullResponse
  ( LiteratureFullResponse(..)
  , literatureFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LiteratureFull as LiteratureFull

data LiteratureFullResponse = LiteratureFullResponse
  { literature :: Maybe LiteratureFull.LiteratureFull -- ^ Full literature, returned when queried using UID
  }
  deriving (Eq, Show)

literatureFullResponseSchema :: FC.Fleece t => FC.Schema t LiteratureFullResponse
literatureFullResponseSchema =
  FC.object $
    FC.constructor LiteratureFullResponse
      #+ FC.optional "literature" literature LiteratureFull.literatureFullSchema