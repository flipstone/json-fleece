{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.ReferenceResponses.Response400Body
  ( Response400Body(..)
  , response400BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.TestCases.ReferenceResponses.Response400Body.Bad as Bad
import qualified TestCases.Operations.TestCases.ReferenceResponses.Response400Body.Error as Error

data Response400Body = Response400Body
  { bad :: Maybe Bad.Bad
  , error :: Error.Error
  }
  deriving (Eq, Show)

response400BodySchema :: FC.Fleece schema => schema Response400Body
response400BodySchema =
  FC.object $
    FC.constructor Response400Body
      #+ FC.optional "bad" bad Bad.badSchema
      #+ FC.required "error" error Error.errorSchema