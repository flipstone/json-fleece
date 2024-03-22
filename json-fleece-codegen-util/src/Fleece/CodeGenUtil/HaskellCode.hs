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
  , quote
  , union
  , unionTypeList
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
  , mapOf
  , maybeOf
  , eitherOf
  , dollar
  , functorMap
  , record
  , delimitLines
  , newtype_
  , deriving_
  , enum
  , sumType
  , typeAnnotate
  , stringLiteral
  , intLiteral
  , caseMatch
  , eqClass
  , showClass
  , ordClass
  , enumClass
  , boundedClass
  , preludeType
  , shrubberyType
  ) where

-- import prelude explicitly since we want to define our own 'lines' function

import Data.Maybe (fromMaybe)
import Prelude (Eq ((==)), Foldable, Int, Maybe (Just, Nothing), Monoid (mempty), Ord, Semigroup ((<>)), String, any, flip, fmap, id, map, maybe, mconcat, show, zip, ($), (+), (-), (.))

import qualified Data.Char as Char
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.NonEmptyText as NET
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
    { typeNameText = transformDigitPrefixes . Manip.toPascal $ t
    , typeNameModule = moduleName
    , typeNameSuggestedQualifier = fmap (transformDigitPrefixes . Manip.toPascal) mbQualifier
    }

toConstructorName :: TypeName -> T.Text -> ConstructorName
toConstructorName typeName constructorName =
  case T.unpack constructorName of
    c : _
      | Char.isNumber c ->
          ConstructorName . fromText $
            Manip.toPascal (typeNameText typeName)
              <> Manip.toPascal constructorName
    _ -> ConstructorName . fromText $ Manip.toPascal constructorName

toModuleName :: T.Text -> ModuleName
toModuleName =
  ModuleName
    . T.intercalate "."
    . map transformDigitPrefixes
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

transformDigitPrefixes :: T.Text -> T.Text
transformDigitPrefixes original =
  let
    digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    hasNumberPrefix = any (flip T.isPrefixOf original) digits
  in
    if hasNumberPrefix
      then "Num" <> original
      else original

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

record ::
  TypeName ->
  [(VarName, TypeExpression, Maybe NET.NonEmptyText)] ->
  Maybe [TypeName] ->
  HaskellCode
record typeName fields mbDeriveClasses =
  let
    mkField :: (VarName, TypeExpression, Maybe NET.NonEmptyText) -> HaskellCode
    mkField (fieldName, fieldType, fieldDescription) =
      typeAnnotate fieldName fieldType
        <> maybe
          ""
          ((" -- ^ " <>) . fromText . T.replace "\n" " " . NET.toText)
          fieldDescription

    fieldLines =
      delimitLines "{ " ", " (map mkField fields)

    derivations =
      deriving_ (fromMaybe [eqClass, showClass] mbDeriveClasses)

    maybeClosingParen =
      if List.null fieldLines
        then []
        else ["}"]
  in
    lines
      ( "data " <> typeNameToCode Nothing typeName <> " = " <> typeNameToCode Nothing typeName
          : map (indent 2) (fieldLines <> maybeClosingParen <> [derivations])
      )

newtype_ :: TypeName -> TypeExpression -> Maybe [TypeName] -> HaskellCode
newtype_ wrapperName baseType mbDeriveClasses =
  lines
    [ "newtype "
        <> typeNameToCode Nothing wrapperName
        <> " = "
        <> typeNameToCode Nothing wrapperName
        <> " "
        <> toCode baseType
    , indent 2 (deriving_ (fromMaybe [showClass, eqClass] mbDeriveClasses))
    ]

deriving_ :: [TypeName] -> HaskellCode
deriving_ classes =
  "deriving (" <> intercalate ", " (map (typeNameToCode Nothing) classes) <> ")"

listOf :: TypeExpression -> TypeExpression
listOf itemName =
  TypeExpression ("[" <> toCode itemName <> "]")

mapType :: TypeName
mapType =
  toTypeName "Data.Map" (Just "Map") "Map"

mapOf :: TypeExpression -> TypeExpression -> TypeExpression
mapOf keyName itemName =
  "("
    <> typeNameToCodeDefaultQualification mapType
    <> " "
    <> keyName
    <> " "
    <> itemName
    <> ")"

union :: TypeExpression
union =
  typeNameToCodeDefaultQualification (shrubberyType "Union")

unionTypeList :: [TypeExpression] -> TypeExpression
unionTypeList members =
  fromCode $
    lines
      ( toCode union
          : map (indent 2 . toCode) (delimitLines "'[ " " , " members <> [" ]"])
      )

quote :: HaskellCode -> HaskellCode
quote code =
  "\"" <> code <> "\""

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
    removeTextInsideChars openChar closeChar t =
      let
        go text numParens =
          case (T.uncons text, numParens) of
            (Nothing, _) -> ""
            (Just (c, rest), n) | c == openChar -> go rest (n + 1)
            (Just (c, rest), n) | c == closeChar -> go rest (n - 1)
            (Just (c, rest), 0) -> T.cons c $ go rest 0
            (Just (_, rest), n) -> go rest n
      in
        go t (0 :: Int)
    removeTextInsideParens = removeTextInsideChars '(' ')'
    removeTextInsideBrackets = removeTextInsideChars '[' ']'
    needsParens =
      T.elem ' '
        . removeTextInsideBrackets
        . removeTextInsideParens
        $ renderText name
  in
    if needsParens
      then fromCode "(" <> name <> fromCode ")"
      else name

dollar :: HaskellCode
dollar =
  addReferences [VarReference "Prelude" Nothing "($)"] "$"

functorMap :: HaskellCode
functorMap =
  addReferences [VarReference "Prelude" Nothing "fmap"] "fmap"

enum :: TypeName -> [ConstructorName] -> Maybe [TypeName] -> HaskellCode
enum typeName constructors mbDeriveClasses =
  let
    constructorLines =
      delimitLines "= " "| " (map toCode constructors)

    derivations =
      deriving_ $
        fromMaybe
          [eqClass, showClass, ordClass, enumClass, boundedClass]
          mbDeriveClasses
  in
    lines
      ( "data " <> typeNameToCode Nothing typeName
          : map (indent 2) (constructorLines <> [derivations])
      )

sumType :: TypeName -> [(ConstructorName, TypeExpression)] -> Maybe [TypeName] -> HaskellCode
sumType typeName constructors mbDeriveClasses =
  let
    mkConstructor (conName, conArgType) =
      toCode conName <> " " <> toCode (guardParens conArgType)

    constructorLines =
      delimitLines "= " "| " (map mkConstructor constructors)

    derivations =
      deriving_ (fromMaybe [eqClass, showClass] mbDeriveClasses)
  in
    lines
      ( "data " <> typeNameToCode Nothing typeName
          : map (indent 2) (constructorLines <> [derivations])
      )

stringLiteral :: T.Text -> HaskellCode
stringLiteral text =
  String.fromString (show text)

intLiteral :: Int -> HaskellCode
intLiteral n =
  String.fromString (show n)

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

shrubberyType :: T.Text -> TypeName
shrubberyType =
  toTypeName "Shrubbery" (Just "Shrubbery")
