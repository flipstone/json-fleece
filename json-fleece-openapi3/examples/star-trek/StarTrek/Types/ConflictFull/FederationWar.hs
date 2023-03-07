{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictFull.FederationWar
  ( FederationWar(..)
  , federationWarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FederationWar = FederationWar Bool
  deriving (Show, Eq)

federationWarSchema :: FC.Fleece schema => schema FederationWar
federationWarSchema =
  FC.coerceSchema FC.boolean