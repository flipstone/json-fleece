{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.EnumIntParam
  ( EnumIntParam(..)
  , enumIntParamSchema
  , enumIntParamToText
  , enumIntParamFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

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

enumIntParamFromText :: T.Text -> Either String EnumIntParam
enumIntParamFromText txt =
  case T.unpack txt of
    "10" -> Either.Right EnumIntParam10
    "20" -> Either.Right EnumIntParam20
    "30" -> Either.Right EnumIntParam30
    v -> Either.Left $ "Unknown EnumIntParam: " <> v

enumIntParamSchema :: FC.Fleece t => FC.Schema t EnumIntParam
enumIntParamSchema =
  FC.boundedEnum enumIntParamToText