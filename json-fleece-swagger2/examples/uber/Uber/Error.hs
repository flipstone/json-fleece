{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Error
  ( Error(..)
  , errorSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.Error.Code (Code, codeSchema)
import Uber.Error.Fields (Fields, fieldsSchema)
import Uber.Error.Message (Message, messageSchema)

data Error = Error
  { message :: Maybe Message
  , code :: Maybe Code
  , fields :: Maybe Fields
  }
  deriving (Eq, Show)

errorSchema :: FC.Fleece schema => schema Error
errorSchema =
  FC.object $
    FC.constructor Error
      #+ FC.optional "message" message messageSchema
      #+ FC.optional "code" code codeSchema
      #+ FC.optional "fields" fields fieldsSchema