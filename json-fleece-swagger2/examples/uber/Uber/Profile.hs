{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Profile
  ( Profile(..)
  , profileSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.Profile.Email (Email, emailSchema)
import Uber.Profile.FirstName (FirstName, firstNameSchema)
import Uber.Profile.LastName (LastName, lastNameSchema)
import Uber.Profile.Picture (Picture, pictureSchema)
import Uber.Profile.PromoCode (PromoCode, promoCodeSchema)

data Profile = Profile
  { lastName :: Maybe LastName -- ^ Last name of the Uber user.
  , picture :: Maybe Picture -- ^ Image URL of the Uber user.
  , promoCode :: Maybe PromoCode -- ^ Promo code of the Uber user.
  , email :: Maybe Email -- ^ Email address of the Uber user
  , firstName :: Maybe FirstName -- ^ First name of the Uber user.
  }
  deriving (Eq, Show)

profileSchema :: FC.Fleece schema => schema Profile
profileSchema =
  FC.object $
    FC.constructor Profile
      #+ FC.optional "last_name" lastName lastNameSchema
      #+ FC.optional "picture" picture pictureSchema
      #+ FC.optional "promo_code" promoCode promoCodeSchema
      #+ FC.optional "email" email emailSchema
      #+ FC.optional "first_name" firstName firstNameSchema