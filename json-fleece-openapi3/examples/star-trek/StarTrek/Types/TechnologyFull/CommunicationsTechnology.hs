{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.CommunicationsTechnology
  ( CommunicationsTechnology(..)
  , communicationsTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CommunicationsTechnology = CommunicationsTechnology Bool
  deriving (Show, Eq)

communicationsTechnologySchema :: FC.Fleece t => FC.Schema t CommunicationsTechnology
communicationsTechnologySchema =
  FC.coerceSchema FC.boolean