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

data Name = Name
  { nameQualification :: Maybe String
  , nameUnqualified :: String
  }
  deriving (Eq, Ord)

instance Show Name where
  show = nameToString

instance String.IsString Name where
  fromString = autoQualifiedName

annotateName :: Name -> String -> Name
annotateName name annotation =
  name
    { nameUnqualified = nameUnqualified name <> " " <> annotation
    }

unqualifiedName :: String -> Name
unqualifiedName n =
  Name
    { nameQualification = Nothing
    , nameUnqualified = n
    }

qualifiedName :: String -> String -> Name
qualifiedName q n =
  Name
    { nameQualification = Just q
    , nameUnqualified = n
    }

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

nameToString :: Name -> String
nameToString name =
  case nameQualification name of
    Just q -> q <> "." <> nameUnqualified name
    Nothing -> nameUnqualified name

defaultSchemaName ::
  (Typeable a) =>
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
