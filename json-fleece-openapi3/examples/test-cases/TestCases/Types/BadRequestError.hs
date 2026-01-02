{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.BadRequestError
  ( BadRequestError(..)
  , badRequestErrorSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.BadRequestError.Error as Error

data BadRequestError = BadRequestError
  { error :: Error.Error
  }
  deriving (Eq, Show)

badRequestErrorSchema :: FC.Fleece t => FC.Schema t BadRequestError
badRequestErrorSchema =
  FC.object $
    FC.constructor BadRequestError
      #+ FC.required "error" error Error.errorSchema