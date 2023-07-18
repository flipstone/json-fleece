{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Reference
  ( Reference(..)
  , referenceSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Reference.ReferenceNumber as ReferenceNumber
import qualified StarTrek.Types.Reference.Uid as Uid
import qualified StarTrek.Types.ReferenceType as ReferenceType

data Reference = Reference
  { uid :: Maybe Uid.Uid -- ^ Reference unique ID
  , referenceNumber :: Maybe ReferenceNumber.ReferenceNumber -- ^ Reference number
  , referenceType :: Maybe ReferenceType.ReferenceType -- ^ Reference type
  }
  deriving (Eq, Show)

referenceSchema :: FC.Fleece schema => schema Reference
referenceSchema =
  FC.object $
    FC.constructor Reference
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "referenceNumber" referenceNumber ReferenceNumber.referenceNumberSchema
      #+ FC.optional "referenceType" referenceType ReferenceType.referenceTypeSchema