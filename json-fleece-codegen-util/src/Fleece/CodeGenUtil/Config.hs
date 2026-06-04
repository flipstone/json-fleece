{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil.Config
  ( Config (..)
  , decoder
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dhall

import qualified Fleece.CodeGenUtil as CGU

data Config = Config
  { codeGenOptions :: CGU.CodeGenOptions
  , inputFileName :: FilePath
  , destination :: FilePath
  }

decoder :: Dhall.Decoder (T.Text -> Config)
decoder =
  Dhall.function Dhall.inject $
    Dhall.record $
      Config
        <$> ( CGU.CodeGenOptions
                <$> Dhall.field "moduleBaseName" Dhall.strictText
                <*> Dhall.field "defaultTypeOptions" typeOptionsDecoder
                <*> Dhall.field "typeOptions" typeOptionsMapDecoder
                <*> Dhall.field "strictAdditionalProperties" Dhall.bool
                <*> Dhall.field "useOptionalNullable" Dhall.bool
                <*> Dhall.field "selectedItems" selectedItemsDecoder
            )
        <*> Dhall.field "inputFileName" Dhall.string
        <*> Dhall.field "destination" Dhall.string

typeOptionsMapDecoder :: Dhall.Decoder (Map.Map T.Text CGU.TypeOptions)
typeOptionsMapDecoder =
  fmap Map.fromList $
    Dhall.list $
      Dhall.record $
        (,)
          <$> Dhall.field "type" Dhall.strictText
          <*> Dhall.field "options" typeOptionsDecoder

typeOptionsDecoder :: Dhall.Decoder CGU.TypeOptions
typeOptionsDecoder =
  Dhall.record $
    CGU.TypeOptions
      <$> Dhall.field "dateTimeFormat" dateTimeFormatDecoder
      <*> Dhall.field "formatSpecifier" formatSpecifierDecoder
      <*> Dhall.field "deriveClasses" deriveClassesDecoder
      <*> Dhall.field "reexportFields" Dhall.bool
      <*> Dhall.field "textLengthHandling" textLengthHandlingDecoder

formatSpecifierDecoder :: Dhall.Decoder (Maybe T.Text)
formatSpecifierDecoder = Dhall.maybe Dhall.strictText

dateTimeFormatDecoder :: Dhall.Decoder CGU.DateTimeFormat
dateTimeFormatDecoder =
  Dhall.union
    ( (fmap (\() -> CGU.UTCTimeFormat) (Dhall.constructor "UTCTime" Dhall.unit))
        <> (fmap (\() -> CGU.ZonedTimeFormat) (Dhall.constructor "ZonedTime" Dhall.unit))
        <> (fmap (\() -> CGU.LocalTimeFormat) (Dhall.constructor "LocalTime" Dhall.unit))
    )

deriveClassesDecoder :: Dhall.Decoder (Maybe [CGU.DerivableClass])
deriveClassesDecoder =
  Dhall.union
    ( (fmap (\() -> Nothing) (Dhall.constructor "Default" Dhall.unit))
        <> (fmap Just (Dhall.constructor "These" (Dhall.list derivableClassDecoder)))
    )

derivableClassDecoder :: Dhall.Decoder CGU.DerivableClass
derivableClassDecoder =
  Dhall.union
    ( (fmap (\() -> CGU.Show) (Dhall.constructor "Show" Dhall.unit))
        <> (fmap (\() -> CGU.Eq) (Dhall.constructor "Eq" Dhall.unit))
        <> (fmap (\() -> CGU.Ord) (Dhall.constructor "Ord" Dhall.unit))
        <> (fmap (\() -> CGU.Enum) (Dhall.constructor "Enum" Dhall.unit))
        <> (fmap (\() -> CGU.Bounded) (Dhall.constructor "Bounded" Dhall.unit))
    )

textLengthHandlingDecoder :: Dhall.Decoder CGU.TextLengthHandling
textLengthHandlingDecoder =
  Dhall.union
    ( (fmap (\() -> CGU.IgnoreTextLength) (Dhall.constructor "Ignore" Dhall.unit))
        <> (fmap (\() -> CGU.NonEmptyTextOnly) (Dhall.constructor "NonEmptyText" Dhall.unit))
        <> (fmap (\() -> CGU.BoundedTextHandling) (Dhall.constructor "BoundedText" Dhall.unit))
    )

selectedItemsDecoder :: Dhall.Decoder CGU.SelectedItems
selectedItemsDecoder =
  Dhall.union
    ( (fmap (\() -> CGU.AllItems) (Dhall.constructor "All" Dhall.unit))
        <> (fmap CGU.SomeItems (Dhall.constructor "Selected" (Dhall.list selectorDecoder)))
    )

selectorDecoder :: Dhall.Decoder CGU.Selector
selectorDecoder =
  Dhall.union
    ( (fmap CGU.OperationId (Dhall.constructor "OperationId" Dhall.strictText))
        <> ( fmap
               (uncurry CGU.PathMethod)
               ( Dhall.constructor
                   "PathMethod"
                   ( Dhall.record $
                       (,)
                         <$> Dhall.field "path" Dhall.strictText
                         <*> Dhall.field "method" Dhall.strictText
                   )
               )
           )
        <> (fmap CGU.Component (Dhall.constructor "Component" Dhall.strictText))
    )
