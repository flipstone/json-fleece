{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.LastPage
  ( LastPage(..)
  , lastPageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LastPage = LastPage Bool
  deriving (Show, Eq)

lastPageSchema :: FC.Fleece schema => schema LastPage
lastPageSchema =
  FC.coerceSchema FC.boolean