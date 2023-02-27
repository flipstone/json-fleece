{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Error
  ( Error(..)
  , errorSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Error.Code (Code, codeSchema)
import StarTrek.Error.Message (Message, messageSchema)

data Error = Error
  { message :: Maybe Message -- ^ Error message
  , code :: Maybe Code -- ^ Error code
  }
  deriving (Eq, Show)

errorSchema :: FC.Fleece schema => schema Error
errorSchema =
  FC.object $
    FC.constructor Error
      #+ FC.optional "message" message messageSchema
      #+ FC.optional "code" code codeSchema