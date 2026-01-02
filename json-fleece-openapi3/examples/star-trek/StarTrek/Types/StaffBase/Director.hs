{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.Director
  ( Director(..)
  , directorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Director = Director Bool
  deriving (Show, Eq)

directorSchema :: FC.Fleece t => FC.Schema t Director
directorSchema =
  FC.coerceSchema FC.boolean