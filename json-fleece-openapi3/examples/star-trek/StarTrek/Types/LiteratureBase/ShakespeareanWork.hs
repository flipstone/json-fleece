{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBase.ShakespeareanWork
  ( ShakespeareanWork(..)
  , shakespeareanWorkSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShakespeareanWork = ShakespeareanWork Bool
  deriving (Show, Eq)

shakespeareanWorkSchema :: FC.Fleece t => FC.Schema t ShakespeareanWork
shakespeareanWorkSchema =
  FC.coerceSchema FC.boolean