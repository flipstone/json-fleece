{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fleece.PrettyPrint
  ( PrettyPrinter
  , prettyPrintLazyText
  , prettyPrintText
  , prettyPrintString
  ) where

import qualified Data.DList as DList
import qualified Data.Foldable as Fold
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import GHC.TypeLits (symbolVal)
import qualified Shrubbery

import qualified Fleece.Core as FC

data PrettyPrinter a
  = PrettyPrinter FC.Name (a -> Pretty)

data Inline
  = Plain LTB.Builder
  | FunctionApplication LTB.Builder Inline
  | Append Inline Inline

instance String.IsString Inline where
  fromString = Plain . String.fromString

instance Semigroup Inline where
  (<>) = Append

data Pretty
  = Inline Inline
  | Block [Pretty]
  | Indent Pretty

prettyToBuilder :: LTB.Builder -> Pretty -> LTB.Builder
prettyToBuilder indentation pretty =
  case pretty of
    Inline inline -> indentation <> inlineToBuilder inline <> newline
    Block pretties -> foldMap (prettyToBuilder indentation) pretties
    Indent indentedPretty ->
      prettyToBuilder (indentation <> "  ") indentedPretty

inlineToBuilder :: Inline -> LTB.Builder
inlineToBuilder inline =
  case inline of
    Plain builder -> builder
    FunctionApplication function contents ->
      case contents of
        Plain plainContents -> function <> " " <> plainContents
        otherContents -> function <> " (" <> inlineToBuilder otherContents <> ")"
    Append left right ->
      inlineToBuilder left <> inlineToBuilder right

prettyPrintLazyText :: PrettyPrinter a -> a -> LT.Text
prettyPrintLazyText (PrettyPrinter _name toPretty) a =
  LTB.toLazyText (prettyToBuilder "" (toPretty a))

prettyPrintText :: PrettyPrinter a -> a -> T.Text
prettyPrintText printer =
  LT.toStrict . prettyPrintLazyText printer

prettyPrintString :: PrettyPrinter a -> a -> String
prettyPrintString printer =
  T.unpack . prettyPrintText printer

instance FC.Fleece PrettyPrinter where
  data Object PrettyPrinter object _a
    = Object (DList.DList (object -> Pretty))

  newtype Field PrettyPrinter object _a
    = Field (object -> Pretty)

  newtype AdditionalFields PrettyPrinter object _a
    = AdditionalFields (object -> Pretty)

  newtype UnionMembers PrettyPrinter _allTypes handledTypes
    = UnionMembers (Shrubbery.BranchBuilder handledTypes Pretty)

  newtype TaggedUnionMembers PrettyPrinter _allTags handledTags
    = TaggedUnionMembers (Shrubbery.TaggedBranchBuilder handledTags (T.Text, DList.DList Pretty))

  schemaName (PrettyPrinter name _toBuilder) =
    name

  number =
    PrettyPrinter "number" $ \scientific ->
      case Scientific.toBoundedInteger scientific :: Maybe Int64 of
        Just n -> showInline n
        Nothing -> showInline scientific

  text =
    PrettyPrinter "text" showInline

  boolean =
    PrettyPrinter "boolean" showInline

  array (PrettyPrinter name itemToPretty) =
    PrettyPrinter (FC.annotateName name "array") $ \array ->
      case Fold.toList array of
        [] -> Inline "[]"
        nonEmptyList ->
          Block
            . map (prefixArrayItem . itemToPretty)
            $ nonEmptyList

  null =
    PrettyPrinter "null" showInline

  nullable (PrettyPrinter name renderNotNull) =
    PrettyPrinter (FC.annotateName name "nullable") $ \mbValue ->
      renderEither showInline renderNotNull mbValue

  required fieldName accessor (PrettyPrinter _name renderField) =
    Field $ \object ->
      prettyPrintField fieldName (renderField (accessor object))

  optional fieldName accessor (PrettyPrinter _name renderField) =
    Field $ \object ->
      prettyPrintField fieldName $
        renderMaybe renderField (accessor object)

  mapField _f (Field renderField) =
    Field renderField

  additionalFields accessor (PrettyPrinter _name renderField) =
    AdditionalFields $ \object ->
      let
        printField (key, value) =
          prettyPrintField
            (T.unpack key)
            (renderField value)

        fields =
          map printField
            . Map.toList
            . accessor
            $ object
      in
        Block
          ( Inline "-- additional fields --"
              : fields
          )

  constructor _f =
    Object DList.empty

  field (Object fields) (Field renderField) =
    Object (DList.snoc fields renderField)

  additional (Object fields) (AdditionalFields renderAdditional) =
    Object (DList.snoc fields renderAdditional)

  objectNamed name (Object fields) =
    PrettyPrinter name $ \object ->
      Block
        [ Inline (Plain (renderName name))
        , Indent (Block (map (\f -> f object) (DList.toList fields)))
        ]

  validateNamed name unvalidate _check (PrettyPrinter _name toPretty) =
    PrettyPrinter name $ \value ->
      prefixConstructor
        (renderName name)
        (toPretty (unvalidate value))

  boundedEnumNamed name toText =
    PrettyPrinter name (showInline . toText)

  unionNamed name (UnionMembers branches) =
    PrettyPrinter name (Shrubbery.dissect (Shrubbery.branchBuild branches))

  unionMemberWithIndex _index (PrettyPrinter _name toPretty) =
    UnionMembers (Shrubbery.singleBranch toPretty)

  unionCombine (UnionMembers leftBranches) (UnionMembers rightBranches) =
    UnionMembers (Shrubbery.appendBranches leftBranches rightBranches)

  taggedUnionNamed name tagProperty (TaggedUnionMembers branches) =
    PrettyPrinter name $ \value ->
      let
        (tagValue, memberFields) =
          Shrubbery.dissectTaggedUnion
            (Shrubbery.taggedBranchBuild branches)
            value

        fields =
          prettyPrintField tagProperty (showInline tagValue)
            : DList.toList memberFields
      in
        Block
          [ Inline (Plain (renderName name))
          , Indent (Block fields)
          ]

  taggedUnionMemberWithTag tag (Object fields) =
    let
      tagValue =
        T.pack (symbolVal tag)
    in
      TaggedUnionMembers (Shrubbery.taggedSingleBranch (\a -> (tagValue, fmap ($ a) fields)))

  taggedUnionCombine (TaggedUnionMembers leftBranches) (TaggedUnionMembers rightBranches) =
    TaggedUnionMembers (Shrubbery.appendTaggedBranches leftBranches rightBranches)

  jsonString schema =
    schema

renderName :: FC.Name -> LTB.Builder
renderName =
  LTB.fromString . FC.nameUnqualified

newline :: LTB.Builder
newline =
  "\n"

showInline :: Show s => s -> Pretty
showInline =
  Inline . Plain . LTB.fromString . show

prettyPrintField :: String -> Pretty -> Pretty
prettyPrintField fieldName prettyField =
  let
    fieldNameEquals =
      Plain (LTB.fromString fieldName) <> " ="
  in
    case prettyField of
      Inline inline -> Inline (fieldNameEquals <> " " <> inline)
      rendered -> Block [Inline fieldNameEquals, Indent rendered]

renderMaybe :: (a -> Pretty) -> Maybe a -> Pretty
renderMaybe renderNotNull mbValue =
  case mbValue of
    Nothing -> Inline "Nothing"
    Just a -> prefixConstructor "Just" (renderNotNull a)

renderEither :: (a -> Pretty) -> (b -> Pretty) -> Either a b -> Pretty
renderEither renderA renderB eitherAB =
  case eitherAB of
    Left a -> prefixConstructor "Left" (renderA a)
    Right b -> prefixConstructor "Right" (renderB b)

prefixConstructor :: LTB.Builder -> Pretty -> Pretty
prefixConstructor constructor pretty =
  case pretty of
    Inline inline -> Inline (FunctionApplication constructor inline)
    rendered -> Block [Inline (Plain constructor), Indent rendered]

prefixArrayItem :: Pretty -> Pretty
prefixArrayItem pretty =
  case pretty of
    Inline rendered -> Inline ("- " <> rendered)
    Block [] -> Block [Inline "-"]
    Block (first : rest) -> Block [prefixArrayItem first, Indent (Block rest)]
    Indent indented -> Indent (prefixArrayItem indented)
