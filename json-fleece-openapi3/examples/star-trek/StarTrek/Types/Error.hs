{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Error
  ( Error(..)
  , errorSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Error.Code as Code
import qualified StarTrek.Types.Error.Message as Message

data Error = Error
  { code :: Maybe Code.Code -- ^ Error code
  , message :: Maybe Message.Message -- ^ Error message
  }
  deriving (Eq, Show)

errorSchema :: FC.Fleece schema => schema Error
errorSchema =
  FC.object $
    FC.constructor Error
      #+ FC.optional "code" code Code.codeSchema
      #+ FC.optional "message" message Message.messageSchema