{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ReexportFieldsExample
  ( ReexportFieldsExample(..)
  , reexportFieldsExampleSchema
  , module TestCases.Types.ReexportFieldsExample.Age
  , module TestCases.Types.ReexportFieldsExample.Name
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.ReexportFieldsExample.Age as Age
import qualified TestCases.Types.ReexportFieldsExample.Name as Name

import TestCases.Types.ReexportFieldsExample.Age
import TestCases.Types.ReexportFieldsExample.Name

data ReexportFieldsExample = ReexportFieldsExample
  { age :: Maybe Age.Age
  , name :: Name.Name
  }
  deriving (Eq, Show)

reexportFieldsExampleSchema :: FC.Fleece t => FC.Schema t ReexportFieldsExample
reexportFieldsExampleSchema =
  FC.object $
    FC.constructor ReexportFieldsExample
      #+ FC.optional "age" age Age.ageSchema
      #+ FC.required "name" name Name.nameSchema