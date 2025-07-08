{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInside
  ( InlineYesAdditionalPropertiesWithOneOfNullablityInside(..)
  , inlineYesAdditionalPropertiesWithOneOfNullablityInsideSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#*), (#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInside.Foobar as Foobar
import qualified TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem as InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem

data InlineYesAdditionalPropertiesWithOneOfNullablityInside = InlineYesAdditionalPropertiesWithOneOfNullablityInside
  { foobar :: Maybe Foobar.Foobar
  , additionalProperties :: (Map.Map T.Text InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem.InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem)
  }
  deriving (Eq, Show)

inlineYesAdditionalPropertiesWithOneOfNullablityInsideSchema :: FC.Fleece schema => schema InlineYesAdditionalPropertiesWithOneOfNullablityInside
inlineYesAdditionalPropertiesWithOneOfNullablityInsideSchema =
  FC.object $
    FC.constructor InlineYesAdditionalPropertiesWithOneOfNullablityInside
      #+ FC.optional "foobar" foobar Foobar.foobarSchema
      #* FC.additionalFields additionalProperties InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem.inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema