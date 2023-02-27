{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Reference
  ( Reference(..)
  , referenceSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Reference.ReferenceNumber (ReferenceNumber, referenceNumberSchema)
import StarTrek.Reference.Uid (Uid, uidSchema)
import StarTrek.ReferenceType (ReferenceType, referenceTypeSchema)

data Reference = Reference
  { referenceNumber :: Maybe ReferenceNumber -- ^ Reference number
  , uid :: Maybe Uid -- ^ Reference unique ID
  , referenceType :: Maybe ReferenceType -- ^ Reference type
  }
  deriving (Eq, Show)

referenceSchema :: FC.Fleece schema => schema Reference
referenceSchema =
  FC.object $
    FC.constructor Reference
      #+ FC.optional "referenceNumber" referenceNumber referenceNumberSchema
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "referenceType" referenceType referenceTypeSchema