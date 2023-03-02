{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Error
  ( Error(..)
  , errorSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Error.Code as Code
import qualified Uber.Error.Fields as Fields
import qualified Uber.Error.Message as Message

data Error = Error
  { message :: Maybe Message.Message
  , code :: Maybe Code.Code
  , fields :: Maybe Fields.Fields
  }
  deriving (Eq, Show)

errorSchema :: FC.Fleece schema => schema Error
errorSchema =
  FC.object $
    FC.constructor Error
      #+ FC.optional "message" message Message.messageSchema
      #+ FC.optional "code" code Code.codeSchema
      #+ FC.optional "fields" fields Fields.fieldsSchema