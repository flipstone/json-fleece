{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFullResponse
  ( ElementFullResponse(..)
  , elementFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ElementFull as ElementFull

data ElementFullResponse = ElementFullResponse
  { element :: Maybe ElementFull.ElementFull -- ^ Full element, returned when queried using UID
  }
  deriving (Eq, Show)

elementFullResponseSchema :: FC.Fleece schema => schema ElementFullResponse
elementFullResponseSchema =
  FC.object $
    FC.constructor ElementFullResponse
      #+ FC.optional "element" element ElementFull.elementFullSchema