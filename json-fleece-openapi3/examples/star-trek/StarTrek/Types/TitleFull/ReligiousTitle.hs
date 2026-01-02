{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleFull.ReligiousTitle
  ( ReligiousTitle(..)
  , religiousTitleSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousTitle = ReligiousTitle Bool
  deriving (Show, Eq)

religiousTitleSchema :: FC.Fleece t => FC.Schema t ReligiousTitle
religiousTitleSchema =
  FC.coerceSchema FC.boolean