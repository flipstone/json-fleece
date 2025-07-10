{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldDescriptions
  ( FieldDescriptions(..)
  , fieldDescriptionsSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.FieldDescriptions.EmptyDescription as EmptyDescription
import qualified TestCases.Types.FieldDescriptions.NoDescription as NoDescription
import qualified TestCases.Types.FieldDescriptions.WithDescription as WithDescription

data FieldDescriptions = FieldDescriptions
  { emptyDescription :: Maybe EmptyDescription.EmptyDescription
  , noDescription :: Maybe NoDescription.NoDescription
  , withDescription :: Maybe WithDescription.WithDescription -- ^ This field has a description
  }
  deriving (Eq, Show)

fieldDescriptionsSchema :: FC.Fleece schema => schema FieldDescriptions
fieldDescriptionsSchema =
  FC.object $
    FC.constructor FieldDescriptions
      #+ FC.optional "emptyDescription" emptyDescription EmptyDescription.emptyDescriptionSchema
      #+ FC.optional "noDescription" noDescription NoDescription.noDescriptionSchema
      #+ FC.optional "withDescription" withDescription WithDescription.withDescriptionSchema