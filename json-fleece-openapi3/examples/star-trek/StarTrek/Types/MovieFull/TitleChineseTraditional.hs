{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleChineseTraditional
  ( TitleChineseTraditional(..)
  , titleChineseTraditionalSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleChineseTraditional = TitleChineseTraditional T.Text
  deriving (Show, Eq)

titleChineseTraditionalSchema :: FC.Fleece t => FC.Schema t TitleChineseTraditional
titleChineseTraditionalSchema =
  FC.coerceSchema FC.text