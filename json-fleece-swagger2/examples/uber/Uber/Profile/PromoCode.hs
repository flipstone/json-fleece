{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile.PromoCode
  ( PromoCode(..)
  , promoCodeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PromoCode = PromoCode Text
  deriving (Show, Eq)

promoCodeSchema :: FC.Fleece schema => schema PromoCode
promoCodeSchema =
  FC.coerceSchema FC.text