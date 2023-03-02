{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase.CommunicationsTechnology
  ( CommunicationsTechnology(..)
  , communicationsTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CommunicationsTechnology = CommunicationsTechnology Bool
  deriving (Show, Eq)

communicationsTechnologySchema :: FC.Fleece schema => schema CommunicationsTechnology
communicationsTechnologySchema =
  FC.coerceSchema FC.boolean