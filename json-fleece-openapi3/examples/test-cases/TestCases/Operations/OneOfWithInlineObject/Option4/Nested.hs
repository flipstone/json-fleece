{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineObject.Option4.Nested
  ( Nested(..)
  , nestedSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.OneOfWithInlineObject.Option4.Nested.Name as Name
import qualified TestCases.Types.OneOfWithInlineObject.Option4.Nested.Number as Number

data Nested = Nested
  { name :: Maybe Name.Name
  , number :: Maybe Number.Number
  }
  deriving (Eq, Show)

nestedSchema :: FC.Fleece t => FC.Schema t Nested
nestedSchema =
  FC.object $
    FC.constructor Nested
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "number" number Number.numberSchema