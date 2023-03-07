{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil.HaskellCode
  ( HaskellCode
  , TypeName
  , typeNameText
  , typeNameModule
  , typeNameSuggestedQualifier
  , typeNameToCode
  , typeNameToCodeDefaultQualification
  , TypeExpression
  , ConstructorName
  , VarName
  , varNameText
  , varNameModule
  , varNameSuggestedQualifier
  , varNameToCode
  , varNameToCodeDefaultQualification
  , ModuleName (..)
  , moduleNameToText
  , ToCode (toCode)
  , FromCode (fromCode)
  , ExternalReference (..)
  , ExternalReferences
  , addReferences
  , references
  , fromText
  , renderLazyText
  , renderText
  , renderString
  , newline
  , intercalate
  , lines
  , indent
  , declarations
  , toTypeName
  , toConstructorName
  , toModuleName
  , toVarName
  , toConstructorVarName
  , listOf
  , maybeOf
  , eitherOf
  , dollar
  , record
  , delimitLines
  , newtype_
  , deriving_
  , enum
  , typeAnnotate
  , stringLiteral
  , caseMatch
  , eqClass
  , showClass
  , ordClass
  , enumClass
  , boundedClass
  , preludeType
  ) where

-- import prelude explicitly since we want to define our own 'lines' function
import Prelude (Eq ((==)), Foldable, Int, Maybe (Just, Nothing), Monoid (mempty), Ord, Semigroup ((<>)), String, fmap, id, map, maybe, mconcat, show, zip, ($), (.))

import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Manipulate as Manip

class ToCode c where
  toCode :: c -> HaskellCode

class FromCode c where
  fromCode :: HaskellCode -> c

addReferences :: (FromCode c, ToCode c) => [ExternalReference] -> c -> c
addReferences refs c =
  let
    code = toCode c
  in
    fromCode $
      code
        { codeReferences = Set.fromList refs <> codeReferences code
        }

references :: ToCode c => c -> ExternalReferences
references =
  codeReferences . toCode

data HaskellCode = HaskellCode
  { codeReferences :: ExternalReferences
  , codeBuilder :: LTB.Builder
  }

data ExternalReference
  = TypeReference ModuleName (Maybe T.Text) T.Text
  | VarReference ModuleName (Maybe T.Text) T.Text
  deriving (Eq, Ord)

type ExternalReferences =
  Set.Set ExternalReference

instance ToCode HaskellCode where
  toCode = id

instance FromCode HaskellCode where
  fromCode = id

instance String.IsString HaskellCode where
  fromString s =
    HaskellCode
      { codeReferences = mempty
      , codeBuilder = String.fromString s
      }

instance Semigroup HaskellCode where
  left <> right =
    HaskellCode
      { codeReferences = codeReferences left <> codeReferences right
      , codeBuilder = codeBuilder left <> codeBuilder right
      }

instance Monoid HaskellCode where
  mempty =
    HaskellCode
      { codeReferences = mempty
      , codeBuilder = mempty
      }

data TypeName = TypeName
  { typeNameText :: T.Text
  , typeNameModule :: ModuleName
  , typeNameSuggestedQualifier :: Maybe T.Text
  }

typeNameToCode :: FromCode c => Maybe T.Text -> TypeName -> c
typeNameToCode mbQualifier typeName =
  let
    nameText =
      typeNameText typeName

    moduleName =
      typeNameModule typeName

    code =
      case mbQualifier of
        Nothing -> fromText nameText
        Just q -> fromText q <> "." <> fromText nameText
  in
    fromCode
      . addReferences [TypeReference moduleName mbQualifier nameText]
      $ code

typeNameToCodeDefaultQualification :: FromCode c => TypeName -> c
typeNameToCodeDefaultQualification typeName =
  typeNameToCode (typeNameSuggestedQualifier typeName) typeName

newtype TypeExpression
  = TypeExpression HaskellCode
  deriving (ToCode, FromCode, Monoid, Semigroup, String.IsString)

newtype ModuleName
  = ModuleName T.Text
  deriving (Eq, Ord, Monoid, String.IsString)

instance Semigroup ModuleName where
  ModuleName left <> ModuleName right =
    ModuleName (left <> "." <> right)

instance ToCode ModuleName where
  toCode (ModuleName m) = fromText m

moduleNameToText :: ModuleName -> T.Text
moduleNameToText (ModuleName text) =
  text

newtype ConstructorName
  = ConstructorName HaskellCode
  deriving (ToCode, FromCode, Monoid, Semigroup)

data VarName = VarName
  { varNameText :: T.Text
  , varNameModule :: ModuleName
  , varNameSuggestedQualifier :: Maybe T.Text
  }
  deriving (Eq, Ord)

varNameToCode :: FromCode c => Maybe T.Text -> VarName -> c
varNameToCode mbQualifier varName =
  let
    nameText =
      varNameText varName

    moduleName =
      varNameModule varName

    code =
      case mbQualifier of
        Nothing -> fromText nameText
        Just q -> fromText q <> "." <> fromText nameText
  in
    fromCode
      . addReferences [VarReference moduleName mbQualifier nameText]
      $ code

varNameToCodeDefaultQualification :: FromCode c => VarName -> c
varNameToCodeDefaultQualification varName =
  varNameToCode (varNameSuggestedQualifier varName) varName

fromText :: FromCode c => T.Text -> c
fromText t =
  fromCode $
    HaskellCode
      { codeReferences = mempty
      , codeBuilder = LTB.fromText t
      }

renderLazyText :: ToCode c => c -> LT.Text
renderLazyText =
  LTB.toLazyText . codeBuilder . toCode

renderText :: ToCode c => c -> T.Text
renderText =
  LT.toStrict . renderLazyText

renderString :: ToCode c => c -> String
renderString =
  T.unpack . renderText

newline :: HaskellCode
newline = "\n"

intercalate :: Foldable f => HaskellCode -> f HaskellCode -> HaskellCode
intercalate sep =
  mconcat . List.intersperse sep . toList

lines :: Foldable f => f HaskellCode -> HaskellCode
lines = intercalate newline

declarations :: Foldable f => f HaskellCode -> HaskellCode
declarations = intercalate (newline <> newline)

typeAnnotate :: VarName -> TypeExpression -> HaskellCode
typeAnnotate item annotation =
  varNameToCode Nothing item <> " :: " <> toCode annotation

indent :: Int -> HaskellCode -> HaskellCode
indent n code =
  fromText (T.replicate n " ") <> code

toTypeName :: ModuleName -> Maybe T.Text -> T.Text -> TypeName
toTypeName moduleName mbQualifier t =
  TypeName
    { typeNameText = Manip.toPascal t
    , typeNameModule = moduleName
    , typeNameSuggestedQualifier = fmap Manip.toPascal mbQualifier
    }

toConstructorName :: T.Text -> ConstructorName
toConstructorName =
  ConstructorName . fromText . Manip.toPascal

toModuleName :: T.Text -> ModuleName
toModuleName =
  ModuleName
    . T.intercalate "."
    . map Manip.toPascal
    . T.splitOn "."

toVarName :: ModuleName -> Maybe T.Text -> T.Text -> VarName
toVarName moduleName mbQualifier t =
  VarName
    { varNameText = mkUnreservedVarName t
    , varNameModule = moduleName
    , varNameSuggestedQualifier = fmap Manip.toPascal mbQualifier
    }

toConstructorVarName :: ModuleName -> Maybe T.Text -> T.Text -> VarName
toConstructorVarName moduleName mbQualifier t =
  VarName
    { varNameText = Manip.toPascal t
    , varNameModule = moduleName
    , varNameSuggestedQualifier = fmap Manip.toPascal mbQualifier
    }

mkUnreservedVarName :: T.Text -> T.Text
mkUnreservedVarName t =
  let
    camelCased =
      Manip.toCamel t
  in
    if Set.member camelCased reservedWords
      then camelCased <> "_"
      else camelCased

reservedWords :: Set.Set T.Text
reservedWords =
  Set.fromList
    [ "case"
    , "class"
    , "data"
    , "deriving"
    , "do"
    , "else"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "of"
    , "module"
    , "newtype"
    , "then"
    , "type"
    , "where"
    ]

delimitLines :: Semigroup c => c -> c -> [c] -> [c]
delimitLines beforeFirst separator =
  let
    mkLine (n, line) =
      let
        prefix =
          if n == (0 :: Int)
            then beforeFirst
            else separator
      in
        prefix <> line
  in
    map mkLine . zip [0 ..]

record :: TypeName -> [(VarName, TypeExpression, Maybe T.Text)] -> HaskellCode
record typeName fields =
  let
    mkField :: (VarName, TypeExpression, Maybe T.Text) -> HaskellCode
    mkField (fieldName, fieldType, fieldDescription) =
      typeAnnotate fieldName fieldType
        <> maybe "" ((" -- ^ " <>) . fromText . T.replace "\n" " ") fieldDescription

    fieldLines =
      delimitLines "{ " ", " (map mkField fields)

    derivations =
      deriving_ [eqClass, showClass]

    maybeClosingParen =
      if List.null fieldLines
        then []
        else ["}"]
  in
    lines
      ( "data " <> typeNameToCode Nothing typeName <> " = " <> typeNameToCode Nothing typeName
          : map (indent 2) (fieldLines <> maybeClosingParen <> [derivations])
      )

newtype_ :: TypeName -> TypeExpression -> HaskellCode
newtype_ wrapperName baseType =
  lines
    [ "newtype "
        <> typeNameToCode Nothing wrapperName
        <> " = "
        <> typeNameToCode Nothing wrapperName
        <> " "
        <> toCode baseType
    , indent 2 (deriving_ [showClass, eqClass])
    ]

deriving_ :: [TypeName] -> HaskellCode
deriving_ classes =
  "deriving (" <> intercalate ", " (map (typeNameToCode Nothing) classes) <> ")"

listOf :: TypeExpression -> TypeExpression
listOf itemName =
  TypeExpression ("[" <> toCode itemName <> "]")

maybeOf :: TypeExpression -> TypeExpression
maybeOf itemName =
  typeNameToCode Nothing (preludeType "Maybe")
    <> fromCode " "
    <> guardParens itemName

eitherOf :: TypeExpression -> TypeExpression -> TypeExpression
eitherOf left right =
  typeNameToCode Nothing (preludeType "Either")
    <> fromCode " "
    <> guardParens left
    <> fromCode " "
    <> guardParens right

guardParens :: TypeExpression -> TypeExpression
guardParens name =
  let
    needsParens =
      T.elem ' ' (renderText name)
  in
    if needsParens
      then fromCode "(" <> name <> fromCode ")"
      else name

dollar :: HaskellCode
dollar =
  addReferences [VarReference "Prelude" Nothing "($)"] "$"

enum :: TypeName -> [ConstructorName] -> HaskellCode
enum typeName constructors =
  let
    constructorLines =
      delimitLines "= " "| " (map toCode constructors)

    derivations =
      deriving_ [eqClass, showClass, ordClass, enumClass, boundedClass]
  in
    lines
      ( "data " <> typeNameToCode Nothing typeName
          : map (indent 2) (constructorLines <> [derivations])
      )

stringLiteral :: T.Text -> HaskellCode
stringLiteral text =
  String.fromString (show text)

caseMatch :: ConstructorName -> HaskellCode -> HaskellCode
caseMatch constructor body =
  toCode constructor <> " -> " <> body

eqClass :: TypeName
eqClass =
  preludeType "Eq"

showClass :: TypeName
showClass =
  preludeType "Show"

ordClass :: TypeName
ordClass =
  preludeType "Ord"

enumClass :: TypeName
enumClass =
  preludeType "Enum"

boundedClass :: TypeName
boundedClass =
  preludeType "Bounded"

preludeType :: T.Text -> TypeName
preludeType =
  toTypeName "Prelude" Nothing
