{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.PromoCode
  ( PromoCode(..)
  , promoCodeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PromoCode = PromoCode T.Text
  deriving (Show, Eq)

promoCodeSchema :: FC.Fleece schema => schema PromoCode
promoCodeSchema =
  FC.coerceSchema FC.text