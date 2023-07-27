let
  DateTimeFormat = < UTCTime | ZonedTime | LocalTime >

let
  DateFormat = < ISO8601Date | CustomDate : Text >

let
  DerivableClass = < Show | Eq | Ord | Enum | Bounded >

let
  DeriveClasses = < Default | These : List DerivableClass >

let
  TypeOptions =
    { Type =
        { dateTimeFormat : DateTimeFormat
        , dateFormat : DateFormat
        , deriveClasses : DeriveClasses
        }
    , default =
        { dateTimeFormat = DateTimeFormat.UTCTime
        , dateFormat = DateFormat.ISO8601Date
        , deriveClasses = DeriveClasses.Default
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
    }

in
  { DateTimeFormat = DateTimeFormat
  , DerivableClass = DerivableClass
  , DeriveClasses = DeriveClasses
  , TypeOptions = TypeOptions
  , SpecificTypeOptions = SpecificTypeOptions
  , baseConfig = baseConfig
  , deriveDefault = DeriveClasses.Default
  , derive = DeriveClasses.These
  , noClasses = ([] : List DerivableClass)
  , utcTime = DateTimeFormat.UTCTime
  , zonedTime = DateTimeFormat.ZonedTime
  , localTime = DateTimeFormat.LocalTime
  , customDate = DateFormat.CustomDate
  , show = DerivableClass.Show
  , eq = DerivableClass.Eq
  , ord = DerivableClass.Ord
  , enum = DerivableClass.Enum
  , bounded = DerivableClass.Bounded
  }
