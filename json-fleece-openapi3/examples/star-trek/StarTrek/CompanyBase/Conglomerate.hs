{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyBase.Conglomerate
  ( Conglomerate(..)
  , conglomerateSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Conglomerate = Conglomerate Bool
  deriving (Show, Eq)

conglomerateSchema :: FC.Fleece schema => schema Conglomerate
conglomerateSchema =
  FC.coerceSchema FC.boolean