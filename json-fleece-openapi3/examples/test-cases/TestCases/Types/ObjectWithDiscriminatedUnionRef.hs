{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ObjectWithDiscriminatedUnionRef
  ( ObjectWithDiscriminatedUnionRef(..)
  , objectWithDiscriminatedUnionRefSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.Baz as Baz

data ObjectWithDiscriminatedUnionRef = ObjectWithDiscriminatedUnionRef
  { bazRef :: Maybe Baz.Baz
  }
  deriving (Eq, Show)

objectWithDiscriminatedUnionRefSchema :: FC.Fleece schema => schema ObjectWithDiscriminatedUnionRef
objectWithDiscriminatedUnionRefSchema =
  FC.object $
    FC.constructor ObjectWithDiscriminatedUnionRef
      #+ FC.optional "bazRef" bazRef Baz.bazSchema