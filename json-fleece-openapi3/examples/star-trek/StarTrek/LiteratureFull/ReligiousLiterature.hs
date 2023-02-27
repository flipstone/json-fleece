{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureFull.ReligiousLiterature
  ( ReligiousLiterature(..)
  , religiousLiteratureSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousLiterature = ReligiousLiterature Bool
  deriving (Show, Eq)

religiousLiteratureSchema :: FC.Fleece schema => schema ReligiousLiterature
religiousLiteratureSchema =
  FC.coerceSchema FC.boolean