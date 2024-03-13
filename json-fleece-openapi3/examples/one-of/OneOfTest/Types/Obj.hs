{-# LANGUAGE NoImplicitPrelude #-}

module OneOfTest.Types.Obj
  ( Obj(..)
  , objSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import qualified OneOfTest.Types.Obj.Foobar as Foobar
import Prelude (($), Eq, Maybe, Show)

data Obj = Obj
  { foobar :: Maybe Foobar.Foobar
  }
  deriving (Eq, Show)

objSchema :: FC.Fleece schema => schema Obj
objSchema =
  FC.object $
    FC.constructor Obj
      #+ FC.optional "foobar" foobar Foobar.foobarSchema