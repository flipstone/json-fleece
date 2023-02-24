{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error
  ( Error(..)
  , errorSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data Error = Error
  { message :: Maybe Text -- ^ Error message
  , code :: Maybe Text -- ^ Error code
  }
  deriving (Eq, Show)

errorSchema :: FC.Fleece schema => schema Error
errorSchema =
  FC.object $
    FC.constructor Error
      #+ FC.optionalField FC.OmitKey_DelegateNull "message" message FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "code" code FC.text