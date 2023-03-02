{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.Director
  ( Director(..)
  , directorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Director = Director Bool
  deriving (Show, Eq)

directorSchema :: FC.Fleece schema => schema Director
directorSchema =
  FC.coerceSchema FC.boolean