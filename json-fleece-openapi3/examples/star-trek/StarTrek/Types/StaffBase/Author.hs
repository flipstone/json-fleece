{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.Author
  ( Author(..)
  , authorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Author = Author Bool
  deriving (Show, Eq)

authorSchema :: FC.Fleece schema => schema Author
authorSchema =
  FC.coerceSchema FC.boolean