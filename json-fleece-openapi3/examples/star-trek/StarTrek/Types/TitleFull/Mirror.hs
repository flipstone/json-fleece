{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleFull.Mirror
  ( Mirror(..)
  , mirrorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Mirror = Mirror Bool
  deriving (Show, Eq)

mirrorSchema :: FC.Fleece t => FC.Schema t Mirror
mirrorSchema =
  FC.coerceSchema FC.boolean