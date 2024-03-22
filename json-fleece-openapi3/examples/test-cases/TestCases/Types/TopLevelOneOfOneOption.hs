{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.TopLevelOneOfOneOption
  ( TopLevelOneOfOneOption(..)
  , topLevelOneOfOneOptionSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery

newtype TopLevelOneOfOneOption = TopLevelOneOfOneOption (Shrubbery.Union
  '[ T.Text
   ])
  deriving (Show, Eq)

topLevelOneOfOneOptionSchema :: FC.Fleece schema => schema TopLevelOneOfOneOption
topLevelOneOfOneOptionSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.TopLevelOneOfOneOption" "TopLevelOneOfOneOption") $
      FC.unionMember FC.text