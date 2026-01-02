{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ObjectWithFieldStartingWithNumber
  ( ObjectWithFieldStartingWithNumber(..)
  , objectWithFieldStartingWithNumberSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.Num2SchemaStartingWithNumber as Num2SchemaStartingWithNumber

data ObjectWithFieldStartingWithNumber = ObjectWithFieldStartingWithNumber
  { num2SchemaStartingWithNumber :: Maybe Num2SchemaStartingWithNumber.Num2SchemaStartingWithNumber -- ^ This Schema's name starts with a number and must be renamed for the generated code to be correct.
  }
  deriving (Eq, Show)

objectWithFieldStartingWithNumberSchema :: FC.Fleece t => FC.Schema t ObjectWithFieldStartingWithNumber
objectWithFieldStartingWithNumberSchema =
  FC.object $
    FC.constructor ObjectWithFieldStartingWithNumber
      #+ FC.optional "2SchemaStartingWithNumber" num2SchemaStartingWithNumber Num2SchemaStartingWithNumber.num2SchemaStartingWithNumberSchema