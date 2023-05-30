{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Baz
  ( Baz(..)
  , bazSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.TestCases.InlineObjectWithPropertiesJsonResponse.Response200Body.Baz.Bax as Bax

data Baz = Baz
  { bax :: Maybe Bax.Bax
  }
  deriving (Eq, Show)

bazSchema :: FC.Fleece schema => schema Baz
bazSchema =
  FC.object $
    FC.constructor Baz
      #+ FC.optional "bax" bax Bax.baxSchema