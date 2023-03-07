{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Profile
  ( Profile(..)
  , profileSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Types.Profile.Email as Email
import qualified Uber.Types.Profile.FirstName as FirstName
import qualified Uber.Types.Profile.LastName as LastName
import qualified Uber.Types.Profile.Picture as Picture
import qualified Uber.Types.Profile.PromoCode as PromoCode

data Profile = Profile
  { lastName :: Maybe LastName.LastName -- ^ Last name of the Uber user.
  , picture :: Maybe Picture.Picture -- ^ Image URL of the Uber user.
  , promoCode :: Maybe PromoCode.PromoCode -- ^ Promo code of the Uber user.
  , email :: Maybe Email.Email -- ^ Email address of the Uber user
  , firstName :: Maybe FirstName.FirstName -- ^ First name of the Uber user.
  }
  deriving (Eq, Show)

profileSchema :: FC.Fleece schema => schema Profile
profileSchema =
  FC.object $
    FC.constructor Profile
      #+ FC.optional "last_name" lastName LastName.lastNameSchema
      #+ FC.optional "picture" picture Picture.pictureSchema
      #+ FC.optional "promo_code" promoCode PromoCode.promoCodeSchema
      #+ FC.optional "email" email Email.emailSchema
      #+ FC.optional "first_name" firstName FirstName.firstNameSchema