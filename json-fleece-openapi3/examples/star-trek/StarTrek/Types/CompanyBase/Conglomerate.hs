{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyBase.Conglomerate
  ( Conglomerate(..)
  , conglomerateSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Conglomerate = Conglomerate Bool
  deriving (Show, Eq)

conglomerateSchema :: FC.Fleece schema => schema Conglomerate
conglomerateSchema =
  FC.coerceSchema FC.boolean