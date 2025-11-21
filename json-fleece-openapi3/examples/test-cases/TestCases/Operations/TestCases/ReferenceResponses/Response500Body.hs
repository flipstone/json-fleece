{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.ReferenceResponses.Response500Body
  ( Response500Body(..)
  , response500BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Operations.TestCases.ReferenceResponses.Response500Body.Error as Error

data Response500Body = Response500Body
  { error :: Error.Error
  }
  deriving (Eq, Show)

response500BodySchema :: FC.Fleece schema => schema Response500Body
response500BodySchema =
  FC.object $
    FC.constructor Response500Body
      #+ FC.required "error" error Error.errorSchema