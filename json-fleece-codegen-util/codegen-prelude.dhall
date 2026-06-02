let
  DateTimeFormat = < UTCTime | ZonedTime | LocalTime >

let
  DerivableClass = < Show | Eq | Ord | Enum | Bounded >

let
  DeriveClasses = < Default | These : List DerivableClass >

let
  TextLengthHandling = < Ignore | NonEmptyText | BoundedText >

let
  Selector = < OperationId : Text | PathMethod : { path : Text, method : Text } | Component : Text >

let
  SelectedItems = < All | Selected : List Selector >

let
  TypeOptions =
    { Type =
        { dateTimeFormat : DateTimeFormat
        , formatSpecifier : Optional Text
        , deriveClasses : DeriveClasses
        , reexportFields : Bool
        , textLengthHandling : TextLengthHandling
        }
    , default =
        { dateTimeFormat = DateTimeFormat.UTCTime
        , formatSpecifier = None Text
        , deriveClasses = DeriveClasses.Default
        , reexportFields = False
        , textLengthHandling = TextLengthHandling.NonEmptyText
        }
    }

let
  SpecificTypeOptions =
    { type : Text
    , options : TypeOptions.Type
    }

let
  baseConfig =
    { defaultTypeOptions = TypeOptions.default
    , typeOptions = [] : List SpecificTypeOptions
    , strictAdditionalProperties = True
    , useOptionalNullable = False
    , selectedItems = SelectedItems.All
    }

in
  { DateTimeFormat = DateTimeFormat
  , DerivableClass = DerivableClass
  , DeriveClasses = DeriveClasses
  , TextLengthHandling = TextLengthHandling
  , Selector = Selector
  , SelectedItems = SelectedItems
  , TypeOptions = TypeOptions
  , SpecificTypeOptions = SpecificTypeOptions
  , baseConfig = baseConfig
  , deriveDefault = DeriveClasses.Default
  , derive = DeriveClasses.These
  , noClasses = ([] : List DerivableClass)
  , utcTime = DateTimeFormat.UTCTime
  , zonedTime = DateTimeFormat.ZonedTime
  , localTime = DateTimeFormat.LocalTime
  , show = DerivableClass.Show
  , eq = DerivableClass.Eq
  , ord = DerivableClass.Ord
  , enum = DerivableClass.Enum
  , bounded = DerivableClass.Bounded
  , ignoreTextLength = TextLengthHandling.Ignore
  , nonEmptyTextOnly = TextLengthHandling.NonEmptyText
  , boundedText = TextLengthHandling.BoundedText
  , allItems = SelectedItems.All
  , selectedItems = SelectedItems.Selected
  , operationId = Selector.OperationId
  , pathMethod = \(path : Text) -> \(method : Text) -> Selector.PathMethod { path = path, method = method }
  , component = Selector.Component
  }
