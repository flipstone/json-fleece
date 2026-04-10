{- | Types and functions for working with schema names. Schema names consist of
an optional module qualification and an unqualified portion.
-}
module Fleece.Core.Name
  ( Name (nameQualification, nameUnqualified)
  , unqualifiedName
  , qualifiedName
  , autoQualifiedName
  , nameToString
  , annotateName
  , defaultSchemaName
  ) where

import qualified Data.String as String
import Data.Typeable (Typeable, tyConModule, typeRep, typeRepTyCon)

{- | A schema name consisting of an optional module qualification and an
unqualified name.
-}
data Name = Name
  { nameQualification :: Maybe String
  -- ^ Optional module qualification prefix for the name.
  , nameUnqualified :: String
  -- ^ The unqualified portion of the name.
  }
  deriving (Eq, Ord)

instance Show Name where
  show = nameToString

instance String.IsString Name where
  fromString = autoQualifiedName

{- | Appends an annotation to a name's unqualified portion, separated by a
space.
-}
annotateName :: Name -> String -> Name
annotateName name annotation =
  name
    { nameUnqualified = nameUnqualified name <> " " <> annotation
    }

-- | Creates an unqualified 'Name' from a string, with no module qualification.
unqualifiedName :: String -> Name
unqualifiedName n =
  Name
    { nameQualification = Nothing
    , nameUnqualified = n
    }

{- | Creates a qualified 'Name' from a module qualification string and an
unqualified name string.
-}
qualifiedName :: String -> String -> Name
qualifiedName q n =
  Name
    { nameQualification = Just q
    , nameUnqualified = n
    }

{- | Parses a dotted string into a qualified 'Name', splitting on the last
@.@ character. For example, @"Foo.Bar.Baz"@ becomes a name with
qualification @"Foo.Bar"@ and unqualified part @"Baz"@.
-}
autoQualifiedName :: String -> Name
autoQualifiedName input =
  let
    splitQual c (mbQ, rest) =
      case mbQ of
        Just q -> (Just (c : q), rest)
        Nothing ->
          case c of
            '.' -> (Just "", rest)
            _ -> (Nothing, c : rest)

    (qualification, name) =
      foldr splitQual (Nothing, []) input
  in
    Name
      { nameQualification = qualification
      , nameUnqualified = name
      }

{- | Converts a 'Name' to its full string representation, including any
qualification prefix separated by a dot.
-}
nameToString :: Name -> String
nameToString name =
  case nameQualification name of
    Just q -> q <> "." <> nameUnqualified name
    Nothing -> nameUnqualified name

{- | Derives a default 'Name' for a schema from a 'Typeable' type, using the
type's module as the qualification and showing the type as the unqualified
name.
-}
defaultSchemaName ::
  Typeable a =>
  schema a ->
  Name
defaultSchemaName schema =
  let
    schemaType =
      typeRep schema

    moduleName =
      tyConModule (typeRepTyCon schemaType)
  in
    Name
      { nameQualification = Just moduleName
      , nameUnqualified = show schemaType
      }
