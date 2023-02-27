{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil.HaskellCode
  ( HaskellCode
  , TypeName
  , ConstructorName
  , VarName
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
  , toQualifiedTypeName
  , toConstructorName
  , toModuleName
  , toVarName
  , toQualifiedVarName
  , varNameForType
  , listOf
  , maybeOf
  , eitherOf
  , dollar
  , record
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
  ) where

import Prelude hiding (lines)

import Data.Foldable (toList)
import Data.Function (on)
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
  = TypeReference ModuleName T.Text
  | VarReference ModuleName T.Text
  | QualifierReference ModuleName T.Text
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

newtype TypeName
  = TypeName HaskellCode
  deriving (ToCode, FromCode, Monoid, Semigroup)

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

newtype VarName
  = VarName HaskellCode
  deriving (ToCode, FromCode, Monoid, Semigroup)

instance Eq VarName where
  (==) = (==) `on` renderText

instance Ord VarName where
  compare = compare `on` renderText

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

typeAnnotate :: ToCode ann => VarName -> ann -> HaskellCode
typeAnnotate item annotation =
  toCode item <> " :: " <> toCode annotation

indent :: Int -> HaskellCode -> HaskellCode
indent n code =
  fromText (T.replicate n " ") <> code

toTypeName :: ModuleName -> T.Text -> TypeName
toTypeName moduleName t =
  let
    pascalCased =
      Manip.toPascal t
  in
    addReferences
      [TypeReference moduleName pascalCased]
      (fromText pascalCased)

toQualifiedTypeName :: ModuleName -> T.Text -> T.Text -> TypeName
toQualifiedTypeName moduleName qualifier t =
  let
    pascalCased =
      qualifier
        <> "."
        <> Manip.toPascal t
  in
    addReferences
      [QualifierReference moduleName qualifier]
      (fromText pascalCased)

toConstructorName :: T.Text -> ConstructorName
toConstructorName =
  ConstructorName . fromText . Manip.toPascal

toModuleName :: T.Text -> ModuleName
toModuleName =
  ModuleName
    . T.intercalate "."
    . map Manip.toPascal
    . T.splitOn "."

toVarName :: ModuleName -> T.Text -> VarName
toVarName moduleName t =
  let
    camelCased =
      mkUnreservedVarName t
  in
    addReferences
      [VarReference moduleName camelCased]
      (fromText camelCased)

toQualifiedVarName :: ModuleName -> T.Text -> T.Text -> VarName
toQualifiedVarName moduleName qualifier t =
  let
    camelCased =
      qualifier <> "." <> mkUnreservedVarName t
  in
    addReferences
      [QualifierReference moduleName qualifier]
      (fromText camelCased)

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

varNameForType :: TypeName -> VarName
varNameForType =
  fromCode . toCode

record :: TypeName -> [(VarName, TypeName, Maybe T.Text)] -> HaskellCode
record typeName fields =
  let
    mkField :: (Int, (VarName, TypeName, Maybe T.Text)) -> HaskellCode
    mkField (n, (fieldName, fieldTypeName, fieldDescription)) =
      let
        prefix =
          if n == 0
            then "{ "
            else ", "
      in
        prefix
          <> typeAnnotate fieldName fieldTypeName
          <> maybe "" ((" -- ^ " <>) . fromText . T.replace "\n" " ") fieldDescription

    fieldLines =
      map mkField (zip [0 ..] fields)

    derivations =
      deriving_ [eqClass, showClass]
  in
    lines
      ( "data " <> toCode typeName <> " = " <> toCode typeName
          : map (indent 2) (fieldLines ++ ["}"] ++ [derivations])
      )

deriving_ :: [TypeName] -> HaskellCode
deriving_ classes =
  "deriving (" <> intercalate ", " (map toCode classes) <> ")"

listOf :: TypeName -> TypeName
listOf itemName =
  TypeName ("[" <> toCode itemName <> "]")

maybeOf :: TypeName -> TypeName
maybeOf itemName =
  toTypeName "Prelude" "Maybe"
    <> fromCode " "
    <> guardParens itemName

eitherOf :: TypeName -> TypeName -> TypeName
eitherOf left right =
  toTypeName "Prelude" "Either"
    <> fromCode " "
    <> guardParens left
    <> fromCode " "
    <> guardParens right

guardParens :: TypeName -> TypeName
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
  addReferences [VarReference "Prelude" "($)"] "$"

enum :: TypeName -> [ConstructorName] -> HaskellCode
enum typeName constructors =
  let
    mkCon :: (Int, ConstructorName) -> HaskellCode
    mkCon (n, conName) =
      let
        prefix =
          if n == 0
            then "= "
            else "| "
      in
        prefix <> toCode conName

    derivations =
      deriving_ [eqClass, showClass, ordClass, enumClass, boundedClass]
  in
    lines
      ( "data " <> toCode typeName
          : map (indent 2) (map mkCon (zip [0 ..] constructors) ++ [derivations])
      )

stringLiteral :: T.Text -> HaskellCode
stringLiteral text =
  String.fromString (show text)

caseMatch :: ConstructorName -> HaskellCode -> HaskellCode
caseMatch constructor body =
  toCode constructor <> " -> " <> body

eqClass :: TypeName
eqClass =
  toTypeName "Prelude" "Eq"

showClass :: TypeName
showClass =
  toTypeName "Prelude" "Show"

ordClass :: TypeName
ordClass =
  toTypeName "Prelude" "Ord"

enumClass :: TypeName
enumClass =
  toTypeName "Prelude" "Enum"

boundedClass :: TypeName
boundedClass =
  toTypeName "Prelude" "Bounded"
