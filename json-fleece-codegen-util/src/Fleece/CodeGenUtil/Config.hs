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
                <*> Dhall.field "ignoreAdditionalProperties" Dhall.bool
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
      <*> Dhall.field "dateFormat" dateFormatDecoder
      <*> Dhall.field "deriveClasses" deriveClassesDecoder

dateFormatDecoder :: Dhall.Decoder CGU.DateFormat
dateFormatDecoder =
  Dhall.union
    ( (fmap (\() -> CGU.ISO8601DateFormat) (Dhall.constructor "ISO8601Date" Dhall.unit))
        <> (fmap CGU.CustomDateFormat (Dhall.constructor "CustomDate" Dhall.strictText))
    )

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
