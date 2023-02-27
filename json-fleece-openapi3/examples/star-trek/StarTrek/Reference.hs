{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Reference
  ( Reference(..)
  , referenceSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ReferenceType (ReferenceType, referenceTypeSchema)

data Reference = Reference
  { referenceNumber :: Maybe Text -- ^ Reference number
  , uid :: Maybe Text -- ^ Reference unique ID
  , referenceType :: Maybe ReferenceType -- ^ Reference type
  }
  deriving (Eq, Show)

referenceSchema :: FC.Fleece schema => schema Reference
referenceSchema =
  FC.object $
    FC.constructor Reference
      #+ FC.optional "referenceNumber" referenceNumber FC.text
      #+ FC.optional "uid" uid FC.text
      #+ FC.optional "referenceType" referenceType referenceTypeSchema