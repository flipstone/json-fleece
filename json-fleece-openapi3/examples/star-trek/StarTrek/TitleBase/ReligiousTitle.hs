{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleBase.ReligiousTitle
  ( ReligiousTitle(..)
  , religiousTitleSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousTitle = ReligiousTitle Bool
  deriving (Show, Eq)

religiousTitleSchema :: FC.Fleece schema => schema ReligiousTitle
religiousTitleSchema =
  FC.coerceSchema FC.boolean