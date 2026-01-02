{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictFull.FederationWar
  ( FederationWar(..)
  , federationWarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FederationWar = FederationWar Bool
  deriving (Show, Eq)

federationWarSchema :: FC.Fleece t => FC.Schema t FederationWar
federationWarSchema =
  FC.coerceSchema FC.boolean