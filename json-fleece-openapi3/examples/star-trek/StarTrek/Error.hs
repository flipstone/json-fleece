{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error
  ( Error(..)
  , errorSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "message" message FC.text
      #+ FC.optional "code" code FC.text