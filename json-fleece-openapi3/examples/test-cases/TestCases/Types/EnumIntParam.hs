{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.EnumIntParam
  ( EnumIntParam(..)
  , enumIntParamSchema
  , enumIntParamToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data EnumIntParam
  = EnumIntParam10
  | EnumIntParam20
  | EnumIntParam30
  deriving (Eq, Show, Ord, Enum, Bounded)

enumIntParamToText :: EnumIntParam -> T.Text
enumIntParamToText v =
  T.pack $
    case v of
      EnumIntParam10 -> "10"
      EnumIntParam20 -> "20"
      EnumIntParam30 -> "30"

enumIntParamSchema :: FC.Fleece t => FC.Schema t EnumIntParam
enumIntParamSchema =
  FC.boundedEnum enumIntParamToText