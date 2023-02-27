{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase.TitleChineseTraditional
  ( TitleChineseTraditional(..)
  , titleChineseTraditionalSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleChineseTraditional = TitleChineseTraditional Text
  deriving (Show, Eq)

titleChineseTraditionalSchema :: FC.Fleece schema => schema TitleChineseTraditional
titleChineseTraditionalSchema =
  FC.coerceSchema FC.text