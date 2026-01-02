{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.Broadcaster
  ( Broadcaster(..)
  , broadcasterSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Broadcaster = Broadcaster Bool
  deriving (Show, Eq)

broadcasterSchema :: FC.Fleece t => FC.Schema t Broadcaster
broadcasterSchema =
  FC.coerceSchema FC.boolean