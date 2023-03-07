{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.Mirror
  ( Mirror(..)
  , mirrorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Mirror = Mirror Bool
  deriving (Show, Eq)

mirrorSchema :: FC.Fleece schema => schema Mirror
mirrorSchema =
  FC.coerceSchema FC.boolean