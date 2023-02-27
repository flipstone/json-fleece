{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull.FederationWar
  ( FederationWar(..)
  , federationWarSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FederationWar = FederationWar Bool
  deriving (Show, Eq)

federationWarSchema :: FC.Fleece schema => schema FederationWar
federationWarSchema =
  FC.coerceSchema FC.boolean