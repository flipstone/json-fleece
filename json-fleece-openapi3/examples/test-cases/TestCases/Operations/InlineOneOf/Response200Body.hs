{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Operations.InlineOneOf.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Show)
import qualified Shrubbery as Shrubbery

newtype Response200Body = Response200Body (Shrubbery.Union
  '[ T.Text
   , Integer
   ])
  deriving (Show, Eq)

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Operations.InlineOneOf.Response200Body" "Response200Body") $
      FC.unionMember FC.text
        #| FC.unionMember FC.integer