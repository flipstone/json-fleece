{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil
  ( generateFleeceCode
  , CodeGenOptions (..)
  , DateTimeFormat (..)
  , TypeOptions (..)
  , DerivableClass (..)
  , lookupTypeOptions
  , CodeGen
  , runCodeGen
  , codeGenError
  , CodeGenResult
  , CodeGenError
  , CodeGenItem (..)
  , CodeGenType (..)
  , CodeGenOperation (..)
  , ResponseStatus (..)
  , CodeGenOperationParam (..)
  , OperationPathPiece (..)
  , OperationParamArity (..)
  , OperationParamType (..)
  , OperationParamLocation (..)
  , CodeGenDataFormat (..)
  , CodeGenObjectField (..)
  , CodeGenAdditionalProperties (..)
  , CodeGenRefType (..)
  , CodeGenKey (..)
  , CodeGenMap
  , SchemaTypeInfo (..)
  , SchemaTypeInfoOrRef
  , CodeSection (Type, Operation)
  , CodeGenUnionMember (..)
  , CodeGenTaggedUnionMember (..)
  , inferSchemaInfoForTypeName
  , inferTypeForInputName
  , arrayLikeTypeInfo
  , mapTypeInfo
  , nullableTypeInfo
  , anyJSONSchemaTypeInfo
  , textFormat
  , textSchemaTypeInfo
  , boolFormat
  , boolSchemaTypeInfo
  , int32Format
  , int32SchemaTypeInfo
  , int64Format
  , int64SchemaTypeInfo
  , integerFormat
  , integerSchemaTypeInfo
  , floatFormat
  , floatSchemaTypeInfo
  , doubleFormat
  , doubleSchemaTypeInfo
  , numberFormat
  , numberSchemaTypeInfo
  , dayFormat
  , utcTimeFormat
  , zonedTimeFormat
  , localTimeFormat
  , enumFormat
  , nullFormat
  , HC.HaskellCode
  , HC.renderLazyText
  , HC.renderText
  , Modules
  ) where

import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.NonEmptyText as NET
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil.HaskellCode as HC

newtype CodeGenError = CodeGenError String
  deriving (Show)

type CodeGen = ReaderT CodeGenOptions (Either CodeGenError)
type CodeGenResult = Either CodeGenError

runCodeGen :: CodeGenOptions -> CodeGen a -> CodeGenResult a
runCodeGen =
  flip runReaderT

type Modules =
  [(FilePath, HC.HaskellCode)]

data CodeGenOptions = CodeGenOptions
  { moduleBaseName :: T.Text
  , defaultTypeOptions :: TypeOptions
  , typeOptionsMap :: Map.Map T.Text TypeOptions
  , strictAdditionalProperties :: Bool
  , useOptionalNullable :: Bool
  }

data TypeOptions = TypeOptions
  { dateTimeFormat :: DateTimeFormat
  , formatSpecifier :: Maybe T.Text
  , deriveClasses :: Maybe [DerivableClass]
  }

deriveClassNames :: TypeOptions -> Maybe [HC.TypeName]
deriveClassNames =
  fmap (map derivableClassTypeName) . deriveClasses

data DerivableClass
  = Show
  | Eq
  | Ord
  | Enum
  | Bounded

derivableClassTypeName :: DerivableClass -> HC.TypeName
derivableClassTypeName derivableClass =
  case derivableClass of
    Show -> HC.showClass
    Eq -> HC.eqClass
    Ord -> HC.ordClass
    Enum -> HC.enumClass
    Bounded -> HC.boundedClass

lookupTypeOptions :: HC.TypeName -> CodeGen TypeOptions
lookupTypeOptions typeName = do
  let
    key =
      HC.moduleNameToText (HC.typeNameModule typeName)
        <> "."
        <> HC.typeNameText typeName

  optionsMap <- asks typeOptionsMap

  case Map.lookup key optionsMap of
    Nothing -> asks defaultTypeOptions
    Just typeOptions -> pure typeOptions

data DateTimeFormat
  = UTCTimeFormat
  | ZonedTimeFormat
  | LocalTimeFormat

codeGenError :: String -> CodeGen a
codeGenError = lift . Left . CodeGenError

data CodeGenKey
  = OperationKey T.Text
  | ParamKey T.Text
  | SchemaKey T.Text
  deriving (Eq, Ord, Show)

type CodeGenMap =
  Map.Map CodeGenKey CodeGenItem

data CodeGenItem
  = CodeGenItemType CodeGenType
  | CodeGenItemOperation CodeGenOperation
  | CodeGenItemOperationParam CodeGenOperationParam

data CodeGenType = CodeGenType
  { codeGenTypeOriginalName :: T.Text
  , codeGenTypeName :: HC.TypeName
  , codeGenTypeSchemaInfo :: SchemaTypeInfo
  , codeGenTypeDescription :: Maybe NET.NonEmptyText
  , codeGenTypeDataFormat :: CodeGenDataFormat
  }

newtype CodeGenTypeReferencesMap
  = CodeGenTypeReferencesMap (Map.Map T.Text (Set.Set ReferenceSource))

instance Semigroup CodeGenTypeReferencesMap where
  (CodeGenTypeReferencesMap left)
    <> (CodeGenTypeReferencesMap right) =
      CodeGenTypeReferencesMap (Map.unionWith Set.union left right)

instance Monoid CodeGenTypeReferencesMap where
  mempty =
    CodeGenTypeReferencesMap Map.empty

mkSingletonReference ::
  ReferenceSource ->
  CodeGenRefType ->
  CodeGenTypeReferencesMap
mkSingletonReference refSource refType =
  let
    refKey typ =
      case typ of
        TypeReference key -> key
        CodeGenRefMap childType -> refKey childType
        CodeGenRefArray _mbMinItems childType -> refKey childType
        CodeGenRefNullable childType -> refKey childType
  in
    CodeGenTypeReferencesMap $
      Map.singleton (refKey refType) (Set.singleton refSource)

lookupReferences :: T.Text -> CodeGenTypeReferencesMap -> Set.Set ReferenceSource
lookupReferences key (CodeGenTypeReferencesMap refMap) =
  Map.findWithDefault Set.empty key refMap

data ReferenceSource
  = ObjectFieldSource
  | ObjectAdditionalPropertiesSource
  | ArrayItemSource
  | NewtypeSource
  | UnionMemberSource
  | TaggedUnionMemberSource T.Text
  deriving (Eq, Ord)

mkReferencesMap :: CodeGenMap -> CodeGenTypeReferencesMap
mkReferencesMap =
  let
    mkFieldReferences =
      mkSingletonReference ObjectFieldSource
        . codeGenFieldType

    mkAdditionalPropertiesReferences =
      either
        mempty
        (mkSingletonReference ObjectAdditionalPropertiesSource)
        . codeGenAdditionalPropertiesSchemaInfoOrRef

    mkUnionMemberReferences =
      either
        mempty
        (mkSingletonReference UnionMemberSource)
        . codeGenUnionMemberType

    mkTaggedUnionMemberReferences discriminatorProperty =
      either
        mempty
        (mkSingletonReference (TaggedUnionMemberSource discriminatorProperty))
        . codeGenTaggedUnionMemberType

    mkTypeReferences codeGenType =
      case codeGenTypeDataFormat codeGenType of
        CodeGenNewType _options typeInfoOrRef ->
          either mempty (mkSingletonReference NewtypeSource) typeInfoOrRef
        CodeGenEnum _options _enumValues -> mempty
        CodeGenObject _options fields mbAdditionalProperties ->
          foldMap mkFieldReferences fields
            <> maybe mempty mkAdditionalPropertiesReferences mbAdditionalProperties
        CodeGenArray _options _mbMaxLength ref ->
          mkSingletonReference ArrayItemSource ref
        CodeGenUnion _options members ->
          foldMap mkUnionMemberReferences members
        CodeGenTaggedUnion discriminatorProperty members ->
          foldMap
            (mkTaggedUnionMemberReferences discriminatorProperty)
            members

    mkOperationReferences _ =
      mempty

    mkParamReferences _ =
      mempty

    mkItemReferences codeGenItem =
      case codeGenItem of
        CodeGenItemType codeGenType -> mkTypeReferences codeGenType
        CodeGenItemOperation codeGenOperation -> mkOperationReferences codeGenOperation
        CodeGenItemOperationParam codeGenOperationParam -> mkParamReferences codeGenOperationParam
  in
    foldMap mkItemReferences . Map.elems

data ResponseStatus
  = ResponseStatusCode Int
  | DefaultResponse
  deriving (Eq)

instance Ord ResponseStatus where
  compare left right =
    case (left, right) of
      -- Ensure that 'DefaultResponse' appears at the _bottom_ of the list
      -- so that any non-default responses have first crack at the processing
      -- the response
      (ResponseStatusCode m, ResponseStatusCode n) -> compare m n
      (ResponseStatusCode _, DefaultResponse) -> LT
      (DefaultResponse, ResponseStatusCode _) -> GT
      (DefaultResponse, DefaultResponse) -> EQ

data CodeGenOperation = CodeGenOperation
  { codeGenOperationOriginalName :: T.Text
  , codeGenOperationMethod :: T.Text
  , codeGenOperationPath :: [OperationPathPiece]
  , codeGenOperationParams :: [CodeGenOperationParam]
  , codeGenOperationRequestBody :: Maybe SchemaTypeInfoOrRef
  , codeGenOperationResponses :: Map.Map ResponseStatus (Maybe SchemaTypeInfoOrRef)
  }

data CodeGenOperationParam = CodeGenOperationParam
  { codeGenOperationParamName :: T.Text
  , codeGenOperationParamModuleName :: HC.ModuleName
  , codeGenOperationParamTypeName :: HC.TypeName
  , codeGenOperationParamDefName :: HC.VarName
  , codeGenOperationParamArity :: OperationParamArity
  , codeGenOperationParamType :: OperationParamType
  , codeGenOperationParamLocation :: OperationParamLocation
  , codeGenOperationParamTypeOptions :: TypeOptions
  }

data OperationPathPiece
  = PathLiteral T.Text
  | PathParamRef T.Text HC.TypeName HC.VarName

data OperationParamArity
  = ExactlyOne
  | AtMostOne
  | AtLeastZero
  | AtLeastOne
  | Cookies
  deriving (Show, Eq)

data OperationParamType
  = ParamTypeString
  | ParamTypeBoolean
  | ParamTypeEnum [T.Text]
  | ParamTypeInteger
  | ParamTypeInt
  | ParamTypeInt8
  | ParamTypeInt16
  | ParamTypeInt32
  | ParamTypeInt64
  | ParamTypeScientific
  | ParamTypeDouble
  | ParamTypeFloat
  | ParamTypeHaskell HC.TypeName

data OperationParamLocation
  = ParamLocationPath
  | ParamLocationQuery
  | ParamLocationHeader
  | ParamLocationCookie
  deriving (Show, Eq)

data CodeGenDataFormat
  = CodeGenNewType TypeOptions SchemaTypeInfoOrRef
  | CodeGenEnum TypeOptions [T.Text]
  | CodeGenObject TypeOptions [CodeGenObjectField] (Maybe CodeGenAdditionalProperties)
  | CodeGenArray TypeOptions (Maybe Integer) CodeGenRefType
  | CodeGenUnion TypeOptions [CodeGenUnionMember]
  | CodeGenTaggedUnion T.Text [CodeGenTaggedUnionMember]

codeGenNewTypeSchemaTypeInfo :: TypeOptions -> SchemaTypeInfo -> CodeGenDataFormat
codeGenNewTypeSchemaTypeInfo typeOptions = CodeGenNewType typeOptions . Left

data CodeGenObjectField = CodeGenObjectField
  { codeGenFieldName :: T.Text
  , codeGenFieldType :: CodeGenRefType
  , codeGenFieldRequired :: Bool
  }

type SchemaTypeInfoOrRef = Either SchemaTypeInfo CodeGenRefType

newtype CodeGenUnionMember = CodeGenUnionMember
  { codeGenUnionMemberType :: SchemaTypeInfoOrRef
  }

newtype CodeGenAdditionalProperties = CodeGenAdditionalProperties
  { codeGenAdditionalPropertiesSchemaInfoOrRef :: SchemaTypeInfoOrRef
  }

data CodeGenTaggedUnionMember = CodeGenTaggedUnionMember
  { codeGenTaggedUnionMemberTag :: T.Text
  , codeGenTaggedUnionMemberType :: SchemaTypeInfoOrRef
  }

data CodeGenRefType
  = TypeReference T.Text
  | CodeGenRefMap CodeGenRefType
  | CodeGenRefArray (Maybe Integer) CodeGenRefType
  | CodeGenRefNullable CodeGenRefType

resolveRefTypeInfo ::
  CodeGenMap ->
  CodeGenRefType ->
  CodeGen SchemaTypeInfo
resolveRefTypeInfo typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          case Map.lookup (SchemaKey ref) typeMap of
            Just (CodeGenItemType codeGenType) ->
              pure (codeGenTypeSchemaInfo codeGenType)
            _ ->
              codeGenError $ "Type " <> show ref <> " not found."
        CodeGenRefArray mbMinItems itemType ->
          fmap (arrayLikeTypeInfo mbMinItems) (go itemType)
        CodeGenRefMap itemType ->
          fmap mapTypeInfo (go itemType)
        CodeGenRefNullable itemType ->
          fmap nullableTypeInfo (go itemType)
  in
    go

resolveFieldDescription ::
  CodeGenMap ->
  CodeGenRefType ->
  Maybe NET.NonEmptyText
resolveFieldDescription typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          case Map.lookup (SchemaKey ref) typeMap of
            Just (CodeGenItemType codeGenType) ->
              codeGenTypeDescription codeGenType
            Just (CodeGenItemOperation _operation) ->
              Nothing
            Just (CodeGenItemOperationParam _param) ->
              Nothing
            Nothing ->
              Nothing
        CodeGenRefArray _mbMinItems itemType ->
          go itemType
        CodeGenRefMap itemType ->
          go itemType
        CodeGenRefNullable itemType ->
          go itemType
  in
    go

schemaInfoOrRefToSchemaTypeInfo ::
  CodeGenMap ->
  SchemaTypeInfoOrRef ->
  CodeGen SchemaTypeInfo
schemaInfoOrRefToSchemaTypeInfo typeMap refOrInfo =
  case refOrInfo of
    Left info -> pure info
    Right refType ->
      resolveRefTypeInfo typeMap refType

dayFormat :: TypeOptions -> CodeGenDataFormat
dayFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions
    $ primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "Day")
    $ case formatSpecifier typeOptions of
      Just formatString ->
        HC.fromCode "("
          <> fleeceCoreFunApp "dayWithFormat" formatString
          <> HC.fromCode ")"
      Nothing -> fleeceCoreVar "day"

utcTimeFormat :: TypeOptions -> CodeGenDataFormat
utcTimeFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions
    $ primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "UTCTime")
    $ case formatSpecifier typeOptions of
      Just formatString ->
        HC.fromCode "("
          <> fleeceCoreFunApp "utcTimeWithFormat" formatString
          <> HC.fromCode ")"
      Nothing -> fleeceCoreVar "utcTime"

zonedTimeFormat :: TypeOptions -> CodeGenDataFormat
zonedTimeFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions
    $ primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "ZonedTime")
    $ case formatSpecifier typeOptions of
      Just formatString ->
        HC.fromCode "("
          <> fleeceCoreFunApp "zonedTimeWithFormat" formatString
          <> HC.fromCode ")"
      Nothing -> fleeceCoreVar "zonedTime"

localTimeFormat :: TypeOptions -> CodeGenDataFormat
localTimeFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions
    $ primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "LocalTime")
    $ case formatSpecifier typeOptions of
      Just formatString ->
        HC.fromCode "("
          <> fleeceCoreFunApp "localTimeWithFormat" formatString
          <> HC.fromCode ")"
      Nothing -> fleeceCoreVar "localTime"

textFormat :: TypeOptions -> CodeGenDataFormat
textFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions textSchemaTypeInfo

textSchemaTypeInfo :: SchemaTypeInfo
textSchemaTypeInfo =
  primitiveSchemaTypeInfo textType (fleeceCoreVar "text")

floatFormat :: TypeOptions -> CodeGenDataFormat
floatFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions floatSchemaTypeInfo

floatSchemaTypeInfo :: SchemaTypeInfo
floatSchemaTypeInfo =
  primitiveSchemaTypeInfo floatType (fleeceCoreVar "float")

doubleFormat :: TypeOptions -> CodeGenDataFormat
doubleFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions doubleSchemaTypeInfo

doubleSchemaTypeInfo :: SchemaTypeInfo
doubleSchemaTypeInfo =
  primitiveSchemaTypeInfo doubleType (fleeceCoreVar "double")

numberFormat :: TypeOptions -> CodeGenDataFormat
numberFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions numberSchemaTypeInfo

numberSchemaTypeInfo :: SchemaTypeInfo
numberSchemaTypeInfo =
  primitiveSchemaTypeInfo scientificType (fleeceCoreVar "number")

int32Format :: TypeOptions -> CodeGenDataFormat
int32Format typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions int32SchemaTypeInfo

int32SchemaTypeInfo :: SchemaTypeInfo
int32SchemaTypeInfo =
  primitiveSchemaTypeInfo int32Type (fleeceCoreVar "int32")

int64Format :: TypeOptions -> CodeGenDataFormat
int64Format typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions int64SchemaTypeInfo

int64SchemaTypeInfo :: SchemaTypeInfo
int64SchemaTypeInfo =
  primitiveSchemaTypeInfo int64Type (fleeceCoreVar "int64")

integerFormat :: TypeOptions -> CodeGenDataFormat
integerFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions integerSchemaTypeInfo

integerSchemaTypeInfo :: SchemaTypeInfo
integerSchemaTypeInfo =
  primitiveSchemaTypeInfo integerType (fleeceCoreVar "integer")

boolFormat :: TypeOptions -> CodeGenDataFormat
boolFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo typeOptions boolSchemaTypeInfo

boolSchemaTypeInfo :: SchemaTypeInfo
boolSchemaTypeInfo =
  primitiveSchemaTypeInfo boolType (fleeceCoreVar "boolean")

enumFormat :: TypeOptions -> [T.Text] -> CodeGenDataFormat
enumFormat =
  CodeGenEnum

nullFormat :: TypeOptions -> CodeGenDataFormat
nullFormat typeOptions =
  codeGenNewTypeSchemaTypeInfo
    typeOptions
    (primitiveSchemaTypeInfo (fleeceCoreType "Null") (fleeceCoreVar "null"))

arrayTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
arrayTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr = HC.listOf (schemaTypeExpr itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "list"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

nonEmptyTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
nonEmptyTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr = HC.nonEmptyOf (schemaTypeExpr itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "nonEmpty"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

arrayLikeTypeInfo :: Maybe Integer -> SchemaTypeInfo -> SchemaTypeInfo
arrayLikeTypeInfo mbMinItems =
  case mbMinItems of
    Just minLength
      | minLength >= 1 ->
          nonEmptyTypeInfo
    _ ->
      arrayTypeInfo

mapTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
mapTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr =
        HC.mapOf
          (HC.typeNameToCodeDefaultQualification textType)
          (schemaTypeExpr itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "map"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

nullableTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
nullableTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr =
        HC.eitherOf
          (HC.typeNameToCodeDefaultQualification (fleeceCoreType "Null"))
          (schemaTypeExpr itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "nullable"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

anyJSONSchemaTypeInfo :: SchemaTypeInfo
anyJSONSchemaTypeInfo =
  SchemaTypeInfo
    { schemaTypeExpr = HC.typeNameToCodeDefaultQualification (fleeceCoreType "AnyJSON")
    , schemaTypeSchema = fleeceCoreVar "anyJSON"
    , schemaTypeObjSchema = fleeceCoreVar "anyJSON"
    }

generateFleeceCode :: CodeGenMap -> CodeGen Modules
generateFleeceCode typeMap =
  let
    refMap =
      mkReferencesMap typeMap
  in
    fmap catMaybes
      . traverse (generateItem typeMap refMap)
      . Map.elems
      $ typeMap

generateItem ::
  CodeGenMap ->
  CodeGenTypeReferencesMap ->
  CodeGenItem ->
  CodeGen (Maybe (FilePath, HC.HaskellCode))
generateItem typeMap refMap codeGenItem =
  case codeGenItem of
    CodeGenItemType codeGenType ->
      Just <$> generateSchemaCode typeMap refMap codeGenType
    CodeGenItemOperation codeGenOperation ->
      Just <$> generateOperationCode typeMap codeGenOperation
    CodeGenItemOperationParam codeGenOperationParam ->
      generateOperationParamCode codeGenOperationParam

generateOperationCode ::
  CodeGenMap ->
  CodeGenOperation ->
  CodeGen (FilePath, HC.HaskellCode)
generateOperationCode typeMap codeGenOperation = do
  moduleName <-
    generatedModuleName
      Operation
      (codeGenOperationOriginalName codeGenOperation)

  beelineMethod <-
    methodToBeelineFunction $
      codeGenOperationMethod codeGenOperation

  let
    responsesTypeName =
      HC.toTypeName moduleName Nothing "Responses"

  responsesTypeOptions <-
    lookupTypeOptions responsesTypeName

  let
    urlPath =
      codeGenOperationPath codeGenOperation

    mkRouteField pathPiece =
      case pathPiece of
        PathParamRef paramNameText paramTypeName _paramDefName ->
          Just
            ( HC.toVarName moduleName Nothing paramNameText
            , HC.typeNameToCodeDefaultQualification paramTypeName
            , Nothing
            )
        PathLiteral _ -> Nothing

    routeFields =
      mapMaybe mkRouteField urlPath

    mbPathParamsTypeName =
      case routeFields of
        [] -> Nothing
        _ -> Just (HC.toTypeName moduleName Nothing "PathParams")

  mbPathParamsDeclaration <-
    traverse
      (recordWithTypeOptions routeFields)
      mbPathParamsTypeName

  let
    queryParams =
      filter (\p -> codeGenOperationParamLocation p == ParamLocationQuery)
        . codeGenOperationParams
        $ codeGenOperation

    headerParams =
      filter (\p -> codeGenOperationParamLocation p == ParamLocationHeader)
        . codeGenOperationParams
        $ codeGenOperation

    cookieParams =
      filter (\p -> codeGenOperationParamLocation p == ParamLocationCookie)
        . codeGenOperationParams
        $ codeGenOperation

  mbQueryParamsCode <-
    case NEL.nonEmpty queryParams of
      Nothing -> pure Nothing
      Just nonEmptyParams ->
        fmap Just $
          mkParameterCollectionCode
            QueryParams
            moduleName
            (HC.toTypeName moduleName Nothing "QueryParams")
            (HC.toVarName moduleName Nothing "queryParamsSchema")
            nonEmptyParams

  mbCookieParamsCode <-
    case NEL.nonEmpty cookieParams of
      Nothing -> pure Nothing
      Just nonEmptyParams ->
        fmap Just $
          mkParameterCollectionCode
            CookieParams
            moduleName
            (HC.toTypeName moduleName Nothing "CookieParams")
            (HC.toVarName moduleName Nothing "cookieParamsSchema")
            nonEmptyParams

  cookieHeaderParams <-
    case mbCookieParamsCode of
      Nothing -> pure []
      Just cookieParamsCode -> do
        typeOptions <- asks defaultTypeOptions
        pure $
          [ CodeGenOperationParam
              { codeGenOperationParamName = "cookies"
              , codeGenOperationParamModuleName = moduleName
              , codeGenOperationParamTypeName = paramCollectionTypeName cookieParamsCode
              , codeGenOperationParamDefName = paramCollectionSchemaName cookieParamsCode
              , codeGenOperationParamArity = Cookies
              , codeGenOperationParamType = ParamTypeHaskell (paramCollectionTypeName cookieParamsCode)
              , codeGenOperationParamLocation = ParamLocationHeader
              , codeGenOperationParamTypeOptions = typeOptions
              }
          ]

  mbHeaderParamsCode <-
    case NEL.nonEmpty (headerParams <> cookieHeaderParams) of
      Nothing -> pure Nothing
      Just nonEmptyParams ->
        fmap Just $
          mkParameterCollectionCode
            HeaderParams
            moduleName
            (HC.toTypeName moduleName Nothing "HeaderParams")
            (HC.toVarName moduleName Nothing "headerParamsSchema")
            nonEmptyParams

  let
    filePath =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    header =
      operationModuleHeader
        moduleName
        mbPathParamsTypeName
        mbQueryParamsCode
        mbHeaderParamsCode
        mbCookieParamsCode

    pathParamsTypeNameAsCode =
      case mbPathParamsTypeName of
        Nothing -> beelineNoPathParamsType
        Just name -> HC.typeNameToCode Nothing name

  mbRequestBodySchemaType <- mapM (schemaInfoOrRefToSchemaTypeInfo typeMap) $ codeGenOperationRequestBody codeGenOperation
  let
    mkRequestBody requestBodySchema =
      beelineRequestBodySchema
        <> " = "
        <> beelineRequestBody
        <> " "
        <> fleeceJSON
        <> " "
        <> HC.toCode (schemaTypeSchema requestBodySchema)

    operationType =
      HC.lines
        . (HC.indent 2 beelineOperation :)
        $ fmap (HC.indent 4)
        $ [ fleeceJSONDecodingError
          , pathParamsTypeNameAsCode
          , maybe
              beelineNoQueryParams
              (HC.typeNameToCode Nothing . paramCollectionTypeName)
              mbQueryParamsCode
          , maybe
              beelineNoHeaderParams
              (HC.typeNameToCode Nothing . paramCollectionTypeName)
              mbHeaderParamsCode
          , maybe
              beelineNoRequestBodyType
              (HC.toCode . schemaTypeExpr)
              mbRequestBodySchemaType
          , HC.typeNameToCode Nothing responsesTypeName
          ]

    operationFields =
      HC.delimitLines "{ " ", " $
        catMaybes
          [ Just (beelineRequestRoute <> " = route")
          , fmap
              (\c -> beelineRequestQuerySchema <> " = " <> paramCollectionSchemaNameAsCode c)
              mbQueryParamsCode
          , fmap
              (\c -> beelineRequestHeaderSchema <> " = " <> paramCollectionSchemaNameAsCode c)
              mbHeaderParamsCode
          , fmap mkRequestBody mbRequestBodySchemaType
          , Just (beelineResponseSchemas <> " = responseSchemas")
          ]

    operation =
      HC.lines
        ( "operation ::"
            : operationType
            : "operation ="
            : HC.indent 2 beelineDefaultOperation
            : map (HC.indent 4) (operationFields <> ["}"])
        )

    mkPiece pathPiece =
      case pathPiece of
        PathLiteral text -> beelinePiece <> " " <> HC.stringLiteral text
        PathParamRef paramNameText _paramTypeName paramDefName ->
          let
            paramName =
              HC.toVarName moduleName Nothing paramNameText
          in
            beelineParam
              <> " "
              <> beelineMkParam
              <> " "
              <> HC.varNameToCodeDefaultQualification paramDefName
              <> " "
              <> HC.varNameToCode Nothing paramName

    route =
      HC.lines
        ( "route :: " <> beelineRouter <> " r => r " <> pathParamsTypeNameAsCode
            : "route ="
            : HC.indent 2 (beelineMethod <> " " <> HC.dollar)
            : HC.indent 4 (beelineMake <> " " <> pathParamsTypeNameAsCode)
            : fmap (\piece -> HC.indent 6 (mkPiece piece)) urlPath
        )

    responseConstructorName responseType responseStatus =
      HC.toConstructorName responseType $
        case responseStatus of
          ResponseStatusCode statusCode ->
            "Response" <> T.pack (show statusCode)
          DefaultResponse ->
            "OtherResponse"

    mkResponseConstructor (responseType, responseStatus, mbSchemaTypeInfo) =
      ( responseConstructorName responseType responseStatus
      , maybe beelineNoResponseBodyType schemaTypeExpr mbSchemaTypeInfo
      )
    statusAndSchemaOrRef =
      Map.toList
        . codeGenOperationResponses
        $ codeGenOperation
    toSchemaTypeInfo (status, schemaOrRef) = do
      schema <- mapM (schemaInfoOrRefToSchemaTypeInfo typeMap) schemaOrRef
      pure (status, schema)

  statusAndSchema <- traverse toSchemaTypeInfo statusAndSchemaOrRef
  let
    responses =
      let
        addResponseType (status, schema) = (responsesTypeName, status, schema)
      in
        map addResponseType statusAndSchema

    responsesType =
      HC.sumType
        responsesTypeName
        (map mkResponseConstructor responses)
        (deriveClassNames responsesTypeOptions)

    responseStatusMacher responseStatus =
      case responseStatus of
        ResponseStatusCode statusCode ->
          beelineStatus <> " " <> HC.intLiteral statusCode
        DefaultResponse ->
          beelineAnyStatus

    schemaTypeResponse schemaTypeInfo =
      beelineResponseBody
        <> " "
        <> fleeceJSON
        <> " "
        <> schemaTypeSchema schemaTypeInfo

    mkResponseSchema (responseType, responseStatus, mbSchemaTypeInfo) =
      "("
        <> responseStatusMacher responseStatus
        <> ", "
        <> HC.functorMap
        <> " "
        <> HC.toCode (responseConstructorName responseType responseStatus)
        <> " ("
        <> maybe beelineNoResponseBody schemaTypeResponse mbSchemaTypeInfo
        <> "))"

    responseSchemaLines =
      HC.delimitLines "[ " ", " $
        map mkResponseSchema responses

    responseSchemas =
      HC.lines
        ( "responseSchemas :: [("
            <> beelineStatusRange
            <> ", "
            <> beelineResponseBodySchema
            <> " "
            <> fleeceJSONDecodingError
            <> " "
            <> HC.typeNameToCode Nothing responsesTypeName
            <> ")]"
            : "responseSchemas ="
            : map (HC.indent 2) (responseSchemaLines <> ["]"])
        )

    moduleBody =
      HC.declarations
        . catMaybes
        $ [ Just operation
          , mbPathParamsDeclaration
          , Just route
          , fmap paramCollectionDeclaration mbQueryParamsCode
          , fmap paramCollectionSchema mbQueryParamsCode
          , fmap paramCollectionDeclaration mbHeaderParamsCode
          , fmap paramCollectionSchema mbHeaderParamsCode
          , fmap paramCollectionDeclaration mbCookieParamsCode
          , fmap paramCollectionSchema mbCookieParamsCode
          , Just responsesType
          , Just responseSchemas
          ]

    pragmas =
      HC.lines
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]

    code =
      HC.declarations $
        [ pragmas
        , header
        , importDeclarations moduleName moduleBody
        , moduleBody
        ]

  pure (filePath, code)

data ParameterCollectionCode = ParameterCollectionCode
  { paramCollectionDeclaration :: HC.HaskellCode
  , paramCollectionSchema :: HC.HaskellCode
  , paramCollectionTypeName :: HC.TypeName
  , paramCollectionSchemaName :: HC.VarName
  }

paramCollectionSchemaNameAsCode :: ParameterCollectionCode -> HC.HaskellCode
paramCollectionSchemaNameAsCode =
  HC.varNameToCode Nothing . paramCollectionSchemaName

data ParamaterCollectionType
  = QueryParams
  | HeaderParams
  | CookieParams

mkParameterCollectionCode ::
  ParamaterCollectionType ->
  HC.ModuleName ->
  HC.TypeName ->
  HC.VarName ->
  NEL.NonEmpty CodeGenOperationParam ->
  CodeGen ParameterCollectionCode
mkParameterCollectionCode collectionType moduleName typeName schemaName params = do
  let
    beelineSchemaClass =
      case collectionType of
        QueryParams -> beelineQuerySchema
        HeaderParams -> beelineHeaderSchema
        CookieParams -> beelineParameterSchema

    paramFieldType param =
      let
        paramTypeName =
          HC.typeNameToCodeDefaultQualification $
            codeGenOperationParamTypeName param
      in
        case codeGenOperationParamArity param of
          ExactlyOne -> paramTypeName
          AtMostOne -> HC.maybeOf paramTypeName
          AtLeastZero -> HC.listOf paramTypeName
          AtLeastOne ->
            HC.typeNameToCodeDefaultQualification HC.nonEmptyType
              <> " "
              <> paramTypeName
          Cookies -> paramTypeName

    mkParamField param =
      ( HC.toVarName moduleName Nothing (codeGenOperationParamName param)
      , paramFieldType param
      , Nothing
      )

    paramFields =
      fmap mkParamField params

    mkParamSchema param =
      let
        paramFieldName =
          HC.varNameToCode Nothing $
            HC.toVarName moduleName Nothing (codeGenOperationParamName param)

        beelineArity =
          case codeGenOperationParamArity param of
            ExactlyOne -> beelineRequired
            AtMostOne -> beelineOptional
            AtLeastZero -> beelineExplodedArray
            AtLeastOne -> beelineExplodedNonEmpty
            Cookies -> beelineCookies
      in
        beelineQueryParam
          <> " "
          <> beelineArity
          <> " "
          <> paramFieldName
          <> " "
          <> HC.varNameToCodeDefaultQualification (codeGenOperationParamDefName param)

    paramsTypeNameAsCode =
      HC.typeNameToCode Nothing typeName

    paramsSchemaType =
      beelineSchemaClass
        <> " p => p "
        <> paramsTypeNameAsCode
        <> " "
        <> paramsTypeNameAsCode

    paramsSchema =
      HC.lines
        ( HC.varNameToCode Nothing schemaName <> " :: " <> paramsSchemaType
            : HC.varNameToCode Nothing schemaName <> " ="
            : HC.indent 2 (beelineMakeParams <> " " <> paramsTypeNameAsCode)
            : map (HC.indent 4 . mkParamSchema) (NEL.toList params)
        )

  paramsDeclaration <- recordWithTypeOptions (NEL.toList paramFields) typeName

  pure $
    ParameterCollectionCode
      { paramCollectionDeclaration = paramsDeclaration
      , paramCollectionSchema = paramsSchema
      , paramCollectionTypeName = typeName
      , paramCollectionSchemaName = schemaName
      }

recordWithTypeOptions ::
  [(HC.VarName, HC.TypeExpression, Maybe NET.NonEmptyText)] ->
  HC.TypeName ->
  CodeGen HC.HaskellCode
recordWithTypeOptions fields name = do
  recordTypeOptions <- lookupTypeOptions name
  pure (HC.record name fields (deriveClassNames recordTypeOptions))

operationModuleHeader ::
  HC.ModuleName ->
  Maybe HC.TypeName ->
  Maybe ParameterCollectionCode ->
  Maybe ParameterCollectionCode ->
  Maybe ParameterCollectionCode ->
  HC.HaskellCode
operationModuleHeader moduleName mbPathParamsTypeName mbQueryParamsCode mbHeaderParamsCode mbCookieParamsCode =
  let
    paramCollectionTypeExport code =
      HC.typeNameToCode Nothing (paramCollectionTypeName code) <> "(..)"

    exports =
      HC.delimitLines "( " ", "
        . catMaybes
        $ [ Just "operation"
          , fmap (\name -> HC.typeNameToCode Nothing name <> "(..)") mbPathParamsTypeName
          , Just "route"
          , fmap paramCollectionTypeExport mbQueryParamsCode
          , fmap paramCollectionSchemaNameAsCode mbQueryParamsCode
          , fmap paramCollectionTypeExport mbHeaderParamsCode
          , fmap paramCollectionSchemaNameAsCode mbHeaderParamsCode
          , fmap paramCollectionTypeExport mbCookieParamsCode
          , fmap paramCollectionSchemaNameAsCode mbCookieParamsCode
          , Just "Responses(..)"
          , Just "responseSchemas"
          ]
  in
    HC.lines
      ( "module " <> HC.toCode moduleName
          : map (HC.indent 2) (exports <> [") where"])
      )

generateOperationParamCode ::
  CodeGenOperationParam ->
  CodeGen (Maybe (FilePath, HC.HaskellCode))
generateOperationParamCode codeGenOperationParam = do
  let
    paramName =
      codeGenOperationParamName codeGenOperationParam

    typeName =
      codeGenOperationParamTypeName codeGenOperationParam

    moduleName =
      codeGenOperationParamModuleName codeGenOperationParam

    filePath =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    paramType =
      codeGenOperationParamType codeGenOperationParam

  case paramTypeToBeelineType moduleName typeName paramType of
    Nothing -> pure Nothing
    Just beelineBaseDef -> do
      let
        typeOptions =
          codeGenOperationParamTypeOptions codeGenOperationParam

        pragmas =
          HC.lines
            [ "{-# LANGUAGE NoImplicitPrelude #-}"
            , "{-# LANGUAGE OverloadedStrings #-}"
            ]

        typeDeclaration =
          if HC.typeNameModule typeName == moduleName
            then case paramTypeToHaskellType paramType of
              Right haskellType ->
                Just $
                  HC.newtype_
                    typeName
                    (HC.fromCode (HC.typeNameToCodeDefaultQualification haskellType))
                    (deriveClassNames typeOptions)
              Left enumValues ->
                Just . snd $ generateEnum typeName enumValues typeOptions
            else Nothing

        defName =
          codeGenOperationParamDefName $
            codeGenOperationParam

        typeNameReference =
          if HC.typeNameModule typeName == moduleName
            then HC.typeNameToCode Nothing typeName
            else HC.typeNameToCodeDefaultQualification typeName

        defDeclaration =
          HC.lines
            [ HC.typeAnnotate defName (beelineParamDef <> " " <> typeNameReference)
            , HC.varNameToCode Nothing defName <> " ="
            , HC.indent 2 (beelineCoerceParam <> " (" <> beelineBaseDef <> " " <> HC.stringLiteral paramName <> ")")
            ]

        header =
          operationParamHeader codeGenOperationParam

        moduleBody =
          HC.declarations
            . catMaybes
            $ [ typeDeclaration
              , Just defDeclaration
              ]

        code =
          HC.declarations $
            [ pragmas
            , header
            , importDeclarations moduleName moduleBody
            , moduleBody
            ]

      pure $ Just (filePath, code)

paramTypeToHaskellType :: OperationParamType -> Either [T.Text] HC.TypeName
paramTypeToHaskellType paramType =
  case paramType of
    ParamTypeString -> Right textType
    ParamTypeBoolean -> Right boolType
    ParamTypeEnum enumValues -> Left enumValues
    ParamTypeInteger -> Right integerType
    ParamTypeInt -> Right intType
    ParamTypeInt8 -> Right int8Type
    ParamTypeInt16 -> Right int16Type
    ParamTypeInt32 -> Right int32Type
    ParamTypeInt64 -> Right int64Type
    ParamTypeScientific -> Right scientificType
    ParamTypeDouble -> Right doubleType
    ParamTypeFloat -> Right floatType
    ParamTypeHaskell haskellType -> Right haskellType

paramTypeToBeelineType ::
  (HC.FromCode c, Semigroup c) =>
  HC.ModuleName ->
  HC.TypeName ->
  OperationParamType ->
  Maybe c
paramTypeToBeelineType moduleName typeName paramType =
  case paramType of
    ParamTypeString -> Just beelineTextParam
    ParamTypeBoolean -> Just beelineBooleanParam
    ParamTypeEnum _ ->
      let
        typeNameText =
          HC.typeNameText typeName

        typeModuleName =
          HC.typeNameModule typeName

        qualifier =
          if moduleName == typeModuleName
            then Nothing
            else Just typeNameText

        toTextName =
          toTextFunctionVarName typeModuleName qualifier typeName
      in
        Just (beelineEnumParam toTextName)
    ParamTypeInteger -> Just beelineIntegerParam
    ParamTypeInt -> Just beelineIntParam
    ParamTypeInt8 -> Just beelineInt8Param
    ParamTypeInt16 -> Just beelineInt16Param
    ParamTypeInt32 -> Just beelineInt32Param
    ParamTypeInt64 -> Just beelineInt64Param
    ParamTypeScientific -> Just beelineScientificParam
    ParamTypeDouble -> Just beelineDoubleParam
    ParamTypeFloat -> Just beelineFloatParam
    ParamTypeHaskell _typeName -> Nothing

operationParamHeader :: CodeGenOperationParam -> HC.HaskellCode
operationParamHeader param =
  let
    moduleName = codeGenOperationParamModuleName param
    typeName = codeGenOperationParamTypeName param
    moduleMatchesTypeName = moduleName == HC.typeNameModule typeName

    typeExport =
      if moduleMatchesTypeName
        then Just (HC.typeNameToCode Nothing typeName <> "(..)")
        else Nothing

    typeToTextExport =
      case codeGenOperationParamType param of
        ParamTypeString ->
          Nothing
        ParamTypeBoolean ->
          Nothing
        ParamTypeEnum _vals ->
          if moduleMatchesTypeName
            then
              Just
                . HC.varNameToCode Nothing
                $ toTextFunctionVarName moduleName Nothing typeName
            else Nothing
        ParamTypeInteger ->
          Nothing
        ParamTypeInt ->
          Nothing
        ParamTypeInt8 ->
          Nothing
        ParamTypeInt16 ->
          Nothing
        ParamTypeInt32 ->
          Nothing
        ParamTypeInt64 ->
          Nothing
        ParamTypeScientific ->
          Nothing
        ParamTypeDouble ->
          Nothing
        ParamTypeFloat ->
          Nothing
        ParamTypeHaskell _typeName ->
          Nothing

    exportLines =
      HC.delimitLines "( " ", " $
        catMaybes
          [ typeExport
          , typeToTextExport
          , Just
              . HC.varNameToCode Nothing
              $ codeGenOperationParamDefName param
          ]
  in
    HC.lines
      ( "module " <> HC.toCode moduleName
          : map (HC.indent 2) (exportLines <> [") where"])
      )

generateCodeGenDataFormat ::
  CodeGenMap ->
  Set.Set ReferenceSource ->
  HC.TypeName ->
  CodeGenDataFormat ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateCodeGenDataFormat typeMap references typeName format = do
  case format of
    CodeGenNewType typeOptions schemaTypeInfoOrRef ->
      generateFleeceNewtype
        typeMap
        typeName
        schemaTypeInfoOrRef
        typeOptions
    CodeGenEnum typeOptions values ->
      pure $ generateFleeceEnum typeName values typeOptions
    CodeGenObject typeOptions fields mbAdditionalProperties ->
      generateFleeceObject typeMap references typeName fields mbAdditionalProperties typeOptions
    CodeGenArray typeOptions mbMinItems itemType ->
      generateFleeceArray typeMap typeName mbMinItems itemType typeOptions
    CodeGenUnion typeOptions members ->
      generateFleeceUnion typeMap typeName members typeOptions
    CodeGenTaggedUnion tagProperty members ->
      generateFleeceTaggedUnion typeMap typeName tagProperty members

requiredPragmasForFormat ::
  CodeGenDataFormat ->
  [HC.HaskellCode]
requiredPragmasForFormat format =
  case format of
    CodeGenNewType _ _ -> []
    CodeGenEnum _ _ -> []
    CodeGenObject _ _ _ -> []
    CodeGenArray _ _ _ -> []
    CodeGenUnion _ _ ->
      [ "{-# LANGUAGE DataKinds #-}"
      ]
    CodeGenTaggedUnion _ _ ->
      [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE ExplicitNamespaces #-}"
      , "{-# LANGUAGE TypeOperators #-}"
      , "{-# LANGUAGE TypeApplications #-}"
      ]

generateSchemaCode ::
  CodeGenMap ->
  CodeGenTypeReferencesMap ->
  CodeGenType ->
  CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode typeMap refMap codeGenType = do
  let
    typeName =
      codeGenTypeName codeGenType

    moduleName =
      HC.typeNameModule typeName

    path =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    format =
      codeGenTypeDataFormat codeGenType

    references =
      lookupReferences
        (codeGenTypeOriginalName codeGenType)
        refMap

  (extraExports, moduleBody) <-
    generateCodeGenDataFormat typeMap references typeName format

  let
    header =
      schemaTypeModuleHeader moduleName typeName extraExports

    pragmas =
      HC.lines
        ( "{-# LANGUAGE NoImplicitPrelude #-}"
            : requiredPragmasForFormat format
        )

    code =
      HC.declarations
        [ pragmas
        , header
        , importDeclarations moduleName moduleBody
        , moduleBody
        ]

  pure (path, code)

schemaTypeModuleHeader ::
  HC.ModuleName -> HC.TypeName -> [HC.VarName] -> HC.HaskellCode
schemaTypeModuleHeader moduleName typeName extraExports =
  let
    schemaName =
      fleeceSchemaNameForType typeName

    exportLines =
      HC.delimitLines
        "( "
        ", "
        ( (HC.typeNameToCode Nothing typeName <> "(..)")
            : HC.varNameToCode Nothing schemaName
            : map (HC.varNameToCode Nothing) extraExports
        )
  in
    HC.lines
      ( "module " <> HC.toCode moduleName
          : map (HC.indent 2) (exportLines <> [") where"])
      )

data ImportItems = ImportItems
  { unqualifiedImports :: Set.Set T.Text
  , qualifiers :: Set.Set T.Text
  }

importName :: T.Text -> ImportItems
importName name =
  ImportItems
    { unqualifiedImports = Set.singleton name
    , qualifiers = mempty
    }

importQualifier :: T.Text -> ImportItems
importQualifier qualifier =
  ImportItems
    { unqualifiedImports = mempty
    , qualifiers = Set.singleton qualifier
    }

unionImports :: ImportItems -> ImportItems -> ImportItems
unionImports left right =
  ImportItems
    { unqualifiedImports = unqualifiedImports left <> unqualifiedImports right
    , qualifiers = qualifiers left <> qualifiers right
    }

importDeclarations ::
  HC.ModuleName ->
  HC.HaskellCode ->
  HC.HaskellCode
importDeclarations thisModuleName code =
  let
    mkImport :: HC.ExternalReference -> Map.Map HC.ModuleName ImportItems
    mkImport ref =
      case ref of
        HC.TypeReference moduleName mbQualifier typeName ->
          if moduleName == thisModuleName
            then Map.empty
            else case mbQualifier of
              Nothing -> Map.singleton moduleName (importName typeName)
              Just qualifier -> Map.singleton moduleName (importQualifier qualifier)
        HC.VarReference moduleName mbQualifier varName ->
          if moduleName == thisModuleName
            then Map.empty
            else case mbQualifier of
              Nothing -> Map.singleton moduleName (importName varName)
              Just qualifier -> Map.singleton moduleName (importQualifier qualifier)

    imports =
      Map.unionsWith
        unionImports
        (map mkImport . Set.toList . HC.references $ code)

    importNames moduleName names =
      case Set.toList names of
        [] ->
          Nothing
        nonEmptyNames ->
          Just $
            "import "
              <> HC.toCode moduleName
              <> " ("
              <> HC.intercalate ", " (map HC.fromText nonEmptyNames)
              <> ")"

    importQualifiedAs moduleName qualifier =
      "import qualified "
        <> HC.toCode moduleName
        <> " as "
        <> HC.fromText qualifier

    mkImportLines (moduleName, importItems) =
      let
        mbUnqualifiedLine =
          importNames moduleName (unqualifiedImports importItems)
        qualifiedLines =
          map (importQualifiedAs moduleName) (Set.toList (qualifiers importItems))
      in
        case mbUnqualifiedLine of
          Nothing -> qualifiedLines
          Just qualifiedLine -> qualifiedLine : qualifiedLines

    importLines =
      foldMap mkImportLines (Map.toList imports)
  in
    HC.lines importLines

generateFleeceNewtype ::
  CodeGenMap ->
  HC.TypeName ->
  SchemaTypeInfoOrRef ->
  TypeOptions ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceNewtype typeMap wrapperName schemaTypeOrRef typeOptions = do
  schemaTypeInfo <- schemaInfoOrRefToSchemaTypeInfo typeMap schemaTypeOrRef
  let
    baseType = schemaTypeExpr schemaTypeInfo
    schemaName = schemaTypeSchema schemaTypeInfo
    newtypeDecl =
      HC.newtype_
        wrapperName
        baseType
        (deriveClassNames typeOptions)

    fleeceSchema =
      fleeceSchemaForType wrapperName $
        [ fleeceCoreVar "coerceSchema" <> " " <> schemaName
        ]

    extraExports =
      []

    body =
      HC.declarations
        [ newtypeDecl
        , fleeceSchema
        ]
  pure (extraExports, body)

generateFleeceEnum ::
  HC.TypeName ->
  [T.Text] ->
  TypeOptions ->
  ([HC.VarName], HC.HaskellCode)
generateFleeceEnum typeName enumValues typeOptions =
  let
    (toTextName, enum) =
      generateEnum typeName enumValues typeOptions

    fleeceSchema =
      fleeceSchemaForType typeName $
        [ fleeceCoreVar "boundedEnum" <> " " <> HC.varNameToCode Nothing toTextName
        ]
  in
    ([toTextName], HC.declarations [enum, fleeceSchema])

generateFleeceUnion ::
  CodeGenMap ->
  HC.TypeName ->
  [CodeGenUnionMember] ->
  TypeOptions ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceUnion typeMap typeName members typeOptions = do
  typeInfos <-
    traverse
      (schemaInfoOrRefToSchemaTypeInfo typeMap . codeGenUnionMemberType)
      members

  let
    moduleName =
      HC.typeNameModule typeName

    mapIgnoringFirst _ [] = []
    mapIgnoringFirst _ [x] = [x]
    mapIgnoringFirst f (x : xs) = x : map f xs

    unionMemberLine schema =
      fleeceCoreVar "unionMember" <> " " <> schema

    unionMemberLines =
      map unionMemberLine $ schemaTypeSchema <$> typeInfos

    unionName =
      fleeceCoreVar "unionNamed"
        <> " ("
        <> fleeceCoreVar "qualifiedName"
        <> " "
        <> HC.quote (HC.toCode moduleName)
        <> " "
        <> HC.quote (HC.typeNameToCode Nothing typeName)
        <> ") "
        <> HC.dollar

    fleeceSchema =
      (if length typeInfos > 1 then HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#|)"] else id) $
        fleeceSchemaForType
          typeName
          ( fleeceCoreVar "coerceSchema" <> " " <> HC.dollar
              : HC.indent 2 unionName
              : map (HC.indent 4) (mapIgnoringFirst (HC.indent 2 . (<>) "#| ") unionMemberLines)
          )

    unionNewType =
      HC.newtype_
        typeName
        ("(" <> HC.union (schemaTypeExpr <$> typeInfos) <> ")")
        (deriveClassNames typeOptions)

    extraExports =
      []

    body =
      HC.declarations
        [ unionNewType
        , fleeceSchema
        ]

  pure (extraExports, body)

generateFleeceTaggedUnion ::
  CodeGenMap ->
  HC.TypeName ->
  T.Text ->
  [CodeGenTaggedUnionMember] ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceTaggedUnion typeMap typeName tagProperty members = do
  let
    mkTaggedTypeInfo taggedUnionMember = do
      typeInfo <-
        schemaInfoOrRefToSchemaTypeInfo typeMap
          . codeGenTaggedUnionMemberType
          $ taggedUnionMember

      pure (codeGenTaggedUnionMemberTag taggedUnionMember, typeInfo)

  taggedTypeInfos <- traverse mkTaggedTypeInfo members

  let
    typeInfos =
      map snd taggedTypeInfos

    moduleName =
      HC.typeNameModule typeName

    mapIgnoringFirst _ [] = []
    mapIgnoringFirst _ [x] = [x]
    mapIgnoringFirst f (x : xs) = x : map f xs

    taggedUnionMemberLine (tag, typeInfo) =
      fleeceCoreVar "taggedUnionMember"
        <> " "
        <> HC.typeApplication (HC.stringLiteral tag)
        <> " "
        <> schemaTypeObjSchema typeInfo

    taggedUnionMemberLines =
      map taggedUnionMemberLine taggedTypeInfos

    taggedUnionName =
      fleeceCoreVar "taggedUnionNamed"
        <> " ("
        <> fleeceCoreVar "qualifiedName"
        <> " "
        <> HC.quote (HC.toCode moduleName)
        <> " "
        <> HC.quote (HC.typeNameToCode Nothing typeName)
        <> ") "
        <> HC.stringLiteral tagProperty
        <> " "
        <> HC.dollar

    fleeceSchema =
      (if length typeInfos > 1 then HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#@)"] else id) $
        fleeceSchemaForType
          typeName
          ( fleeceCoreVar "coerceSchema" <> " " <> HC.dollar
              : HC.indent 2 taggedUnionName
              : map (HC.indent 4) (mapIgnoringFirst (HC.indent 2 . (<>) "#@ ") taggedUnionMemberLines)
          )

    taggedUnionNewType =
      HC.newtype_
        typeName
        ("(" <> HC.taggedUnion (fmap (fmap schemaTypeExpr) taggedTypeInfos) <> ")")
        Nothing

    extraExports =
      []

    body =
      HC.declarations
        [ taggedUnionNewType
        , fleeceSchema
        ]

  pure (extraExports, body)

determineDiscriminatorPropertyForObject ::
  Set.Set ReferenceSource ->
  CodeGen (Maybe T.Text)
determineDiscriminatorPropertyForObject refSet =
  let
    toDiscriminator source =
      case source of
        ObjectFieldSource -> Nothing
        ObjectAdditionalPropertiesSource -> Nothing
        ArrayItemSource -> Nothing
        NewtypeSource -> Nothing
        UnionMemberSource -> Nothing
        TaggedUnionMemberSource property -> Just property
  in
    case catMaybes $ Set.toList (Set.map toDiscriminator refSet) of
      [] -> pure Nothing
      [oneDiscriminator] -> pure $ Just oneDiscriminator
      multiple ->
        -- This limitation could probably be lifted with some additional care
        codeGenError $
          "Objects referenced from tagged unions must have consistent discriminator properties."
            <> " Found the following discriminator properties: "
            <> show multiple

generateFleeceObject ::
  CodeGenMap ->
  Set.Set ReferenceSource ->
  HC.TypeName ->
  [CodeGenObjectField] ->
  Maybe CodeGenAdditionalProperties ->
  TypeOptions ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceObject typeMap references typeName rawCodeGenFields mbAdditionalProperties typeOptions = do
  let
    moduleName =
      HC.typeNameModule typeName

    additionalPropsFieldName =
      HC.toVarName moduleName Nothing "additionalProperties"

  mbDiscriminator <- determineDiscriminatorPropertyForObject references

  let
    codeGenFields =
      case mbDiscriminator of
        Nothing -> rawCodeGenFields
        Just discriminatorName ->
          filter
            (\f -> codeGenFieldName f /= discriminatorName)
            rawCodeGenFields

  fields <-
    traverse (mkFleeceSchemaField typeMap moduleName) codeGenFields

  mbAdditionalPropertiesSchemaTypeInfo <- mapM (schemaInfoOrRefToSchemaTypeInfo typeMap . codeGenAdditionalPropertiesSchemaInfoOrRef) mbAdditionalProperties
  useOptionalNullableBool <- asks useOptionalNullable

  let
    additionalPropsFieldNameAndType =
      case mbAdditionalPropertiesSchemaTypeInfo of
        Nothing ->
          []
        Just additionalPropsTypeInfo ->
          [
            ( additionalPropsFieldName
            , schemaTypeExpr (mapTypeInfo additionalPropsTypeInfo)
            , Nothing
            )
          ]

    fieldNameAndType field =
      (fieldName field, fieldTypeName field, fieldDescription field)

    recordDecl =
      HC.record
        typeName
        (map fieldNameAndType fields ++ additionalPropsFieldNameAndType)
        (deriveClassNames typeOptions)

    fleeceField field =
      HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#+)"] $
        "#+ "
          <> fleeceFieldFunction field useOptionalNullableBool
          <> " "
          <> HC.stringLiteral (fieldJSONName field)
          <> " "
          <> HC.varNameToCode Nothing (fieldName field)
          <> " "
          <> fieldFleeceSchemaCode field

    fleeceAdditionalProps =
      case mbAdditionalPropertiesSchemaTypeInfo of
        Nothing ->
          []
        Just additionalPropsTypeInfo ->
          [ HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#*)"] $
              "#* "
                <> fleeceCoreVar "additionalFields"
                <> " "
                <> HC.varNameToCode Nothing additionalPropsFieldName
                <> " "
                <> schemaTypeSchema additionalPropsTypeInfo
          ]

    fleeceFields =
      map fleeceField fields ++ fleeceAdditionalProps

    fleeceSchemas =
      case mbDiscriminator of
        Nothing ->
          [ fleeceSchemaForType
              typeName
              ( fleeceCoreVar "object" <> " " <> HC.dollar
                  : "  " <> fleeceCoreVar "constructor" <> " " <> HC.typeNameToCode Nothing typeName
                  : map (HC.indent 4) fleeceFields
              )
          ]
        Just _ ->
          [ fleeceSchemaForType
              typeName
              [fleeceCoreVar "object" <> " " <> HC.varNameToCode Nothing (fleeceObjSchemaNameForType typeName)]
          , fleeceObjectForType
              typeName
              ( fleeceCoreVar "constructor" <> " " <> HC.typeNameToCode Nothing typeName
                  : map (HC.indent 2) fleeceFields
              )
          ]

    extraExports =
      case mbDiscriminator of
        Nothing -> []
        Just _ ->
          [fleeceObjSchemaNameForType typeName]

    body =
      HC.declarations $
        [ recordDecl
        ]
          <> fleeceSchemas

  pure (extraExports, body)

generateFleeceArray ::
  CodeGenMap ->
  HC.TypeName ->
  Maybe Integer ->
  CodeGenRefType ->
  TypeOptions ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceArray typeMap typeName mbMinItems itemType typeOptions = do
  let
    elemTypeInfo = arrayLikeTypeInfo mbMinItems
  typeInfo <- fmap elemTypeInfo (resolveRefTypeInfo typeMap itemType)
  generateFleeceNewtype
    typeMap
    typeName
    (Left typeInfo)
    typeOptions

data FleeceSchemaField = FleeceSchemaField
  { fieldName :: HC.VarName
  , fieldJSONName :: T.Text
  , fieldCodeGenField :: CodeGenObjectField
  , fieldBaseSchemaTypeInfo :: SchemaTypeInfo
  , fieldDescription :: Maybe NET.NonEmptyText
  }

fieldRequired :: FleeceSchemaField -> Bool
fieldRequired =
  codeGenFieldRequired . fieldCodeGenField

fieldBaseTypeExpr :: FleeceSchemaField -> HC.TypeExpression
fieldBaseTypeExpr =
  schemaTypeExpr . fieldBaseSchemaTypeInfo

fieldTypeName :: FleeceSchemaField -> HC.TypeExpression
fieldTypeName field =
  let
    baseTypeExpression =
      fieldBaseTypeExpr field
  in
    if fieldRequired field
      then baseTypeExpression
      else HC.maybeOf baseTypeExpression

fieldFleeceSchemaCode :: FleeceSchemaField -> HC.HaskellCode
fieldFleeceSchemaCode =
  schemaTypeSchema . fieldBaseSchemaTypeInfo

fleeceFieldFunction :: FleeceSchemaField -> Bool -> HC.HaskellCode
fleeceFieldFunction field useOptionalNullableBool
  | fieldRequired field = fleeceCoreVar "required"
  | useOptionalNullableBool = fleeceCoreVar "optionalNullable" <> " " <> fleeceCoreConstructorVar "OmitKey"
  | otherwise = fleeceCoreVar "optional"

mkFleeceSchemaField ::
  CodeGenMap ->
  HC.ModuleName ->
  CodeGenObjectField ->
  CodeGen FleeceSchemaField
mkFleeceSchemaField typeMap moduleName codeGenField = do
  let
    name =
      codeGenFieldName codeGenField

    fieldType =
      codeGenFieldType codeGenField

  typeInfo <- resolveRefTypeInfo typeMap fieldType

  pure $
    FleeceSchemaField
      { fieldName = HC.toVarName moduleName Nothing name
      , fieldCodeGenField = codeGenField
      , fieldBaseSchemaTypeInfo = typeInfo
      , fieldJSONName = name
      , fieldDescription = resolveFieldDescription typeMap fieldType
      }

data SchemaTypeInfo = SchemaTypeInfo
  { schemaTypeExpr :: HC.TypeExpression
  , schemaTypeSchema :: HC.HaskellCode
  , schemaTypeObjSchema :: HC.HaskellCode
  }

primitiveSchemaTypeInfo :: HC.TypeName -> HC.HaskellCode -> SchemaTypeInfo
primitiveSchemaTypeInfo typeName schema =
  SchemaTypeInfo
    { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
    , schemaTypeSchema = schema
    , schemaTypeObjSchema = schema
    }

inferSchemaInfoForTypeName :: HC.TypeName -> CodeGen SchemaTypeInfo
inferSchemaInfoForTypeName typeName =
  pure $
    SchemaTypeInfo
      { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
      , schemaTypeSchema = HC.varNameToCodeDefaultQualification $ fleeceSchemaNameForType typeName
      , schemaTypeObjSchema = HC.varNameToCodeDefaultQualification $ fleeceObjSchemaNameForType typeName
      }

inferTypeForInputName :: CodeSection -> T.Text -> CodeGen (HC.ModuleName, HC.TypeName)
inferTypeForInputName section inputName = do
  moduleName <- generatedModuleName section inputName
  case reverse (T.splitOn "." inputName) of
    [] ->
      codeGenError $ "Unable to determine type name for: " <> show inputName
    ("" : _) ->
      codeGenError $ "Unable to determine type name for: " <> show inputName
    (lastPart : _) ->
      pure (moduleName, HC.toTypeName moduleName (Just lastPart) lastPart)

fleeceSchemaNameForType :: HC.TypeName -> HC.VarName
fleeceSchemaNameForType typeName =
  HC.toVarName
    (HC.typeNameModule typeName)
    (HC.typeNameSuggestedQualifier typeName)
    (HC.typeNameText typeName <> "Schema")

fleeceObjSchemaNameForType :: HC.TypeName -> HC.VarName
fleeceObjSchemaNameForType typeName =
  HC.toVarName
    (HC.typeNameModule typeName)
    (HC.typeNameSuggestedQualifier typeName)
    (HC.typeNameText typeName <> "ObjSchema")

fleeceSchemaForType :: HC.TypeName -> [HC.HaskellCode] -> HC.HaskellCode
fleeceSchemaForType typeName bodyLines =
  let
    schemaName =
      fleeceSchemaNameForType typeName

    declType =
      HC.typeAnnotate schemaName $
        HC.typeNameToCodeDefaultQualification fleeceClass
          <> " schema => schema "
          <> HC.typeNameToCode Nothing typeName

    declImpl =
      HC.varNameToCode Nothing schemaName <> " ="
  in
    HC.lines
      ( declType
          : declImpl
          : map (HC.indent 2) bodyLines
      )

fleeceObjectForType :: HC.TypeName -> [HC.HaskellCode] -> HC.HaskellCode
fleeceObjectForType typeName bodyLines =
  let
    schemaName =
      fleeceObjSchemaNameForType typeName

    declType =
      HC.typeAnnotate schemaName $
        HC.typeNameToCodeDefaultQualification fleeceClass
          <> " schema => "
          <> HC.typeNameToCode Nothing fleeceObject
          <> " schema "
          <> HC.typeNameToCode Nothing typeName
          <> " "
          <> HC.typeNameToCode Nothing typeName

    declImpl =
      HC.varNameToCode Nothing schemaName <> " ="
  in
    HC.lines
      ( declType
          : declImpl
          : map (HC.indent 2) bodyLines
      )

generateEnum ::
  HC.TypeName ->
  [T.Text] ->
  TypeOptions ->
  (HC.VarName, HC.HaskellCode)
generateEnum typeName enumValues typeOptions =
  let
    mkEnumItem t =
      (t, HC.toConstructorName typeName t)

    enumItems =
      map mkEnumItem enumValues

    moduleName =
      HC.typeNameModule typeName

    enumDeclaration =
      HC.enum
        typeName
        (map snd enumItems)
        (deriveClassNames typeOptions)

    toTextName =
      toTextFunctionVarName moduleName Nothing typeName

    toTextType =
      HC.typeNameToCode Nothing typeName
        <> " -> "
        <> HC.typeNameToCodeDefaultQualification textType

    toText =
      HC.lines
        ( HC.typeAnnotate toTextName toTextType
            : HC.varNameToCode Nothing toTextName <> " v ="
            : HC.indent 2 (HC.varNameToCodeDefaultQualification textPack <> " " <> HC.dollar)
            : HC.indent 4 "case v of"
            : map (HC.indent 6 . mkToTextCase) enumItems
        )

    mkToTextCase (text, constructor) =
      HC.caseMatch constructor (HC.stringLiteral text)

    body =
      HC.declarations
        [ enumDeclaration
        , toText
        ]
  in
    (toTextName, body)

data CodeSection
  = Type
  | Operation

sectionModule :: CodeSection -> HC.ModuleName
sectionModule section =
  case section of
    Type -> HC.ModuleName "Types"
    Operation -> HC.ModuleName "Operations"

generatedModuleName :: CodeSection -> T.Text -> CodeGen HC.ModuleName
generatedModuleName section text = do
  options <- ask
  pure $
    HC.ModuleName (moduleBaseName options)
      <> sectionModule section
      <> HC.toModuleName text

textType :: HC.TypeName
textType =
  HC.toTypeName "Data.Text" (Just "T") "Text"

textPack :: HC.VarName
textPack =
  HC.toVarName "Data.Text" (Just "T") "pack"

floatType :: HC.TypeName
floatType =
  HC.preludeType "Float"

doubleType :: HC.TypeName
doubleType =
  HC.preludeType "Double"

scientificType :: HC.TypeName
scientificType =
  HC.toTypeName "Data.Scientific" (Just "Sci") "Scientific"

intType :: HC.TypeName
intType =
  HC.preludeType "Int"

int8Type :: HC.TypeName
int8Type =
  HC.toTypeName "Data.Int" (Just "I") "Int8"

int16Type :: HC.TypeName
int16Type =
  HC.toTypeName "Data.Int" (Just "I") "Int16"

int32Type :: HC.TypeName
int32Type =
  HC.toTypeName "Data.Int" (Just "I") "Int32"

int64Type :: HC.TypeName
int64Type =
  HC.toTypeName "Data.Int" (Just "I") "Int64"

integerType :: HC.TypeName
integerType =
  HC.preludeType "Integer"

boolType :: HC.TypeName
boolType =
  HC.preludeType "Bool"

fleeceClass :: HC.TypeName
fleeceClass =
  fleeceCoreType "Fleece"

fleeceObject :: HC.TypeName
fleeceObject =
  fleeceCoreType "Object"

fleeceCoreType :: T.Text -> HC.TypeName
fleeceCoreType =
  HC.toTypeName "Fleece.Core" (Just "FC")

fleeceCoreVar :: HC.FromCode c => T.Text -> c
fleeceCoreVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Fleece.Core" (Just "FC")

fleeceCoreConstructorVar :: HC.FromCode c => T.Text -> c
fleeceCoreConstructorVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Fleece.Core" (Just "FC")

fleeceCoreFunApp :: HC.FromCode c => T.Text -> T.Text -> c
fleeceCoreFunApp functionName argument =
  let
    functionVar =
      HC.varNameToCodeDefaultQualification $
        HC.toVarName "Fleece.Core" (Just "FC") functionName
  in
    HC.fromCode $ functionVar <> HC.fromText " " <> HC.stringLiteral argument

fleeceJSON :: HC.FromCode c => c
fleeceJSON =
  fleeceAesonBeelineConstructor "JSON"

fleeceAesonBeelineConstructor :: HC.FromCode c => T.Text -> c
fleeceAesonBeelineConstructor =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Fleece.Aeson.Beeline" (Just "FA")

fleeceJSONDecodingError :: HC.FromCode c => c
fleeceJSONDecodingError =
  fleeceAesonBeelineConstructor "JSONDecodingError"

methodToBeelineFunction :: HC.FromCode c => T.Text -> CodeGen c
methodToBeelineFunction method =
  fmap beelineRoutingVar $
    case T.toLower method of
      "get" -> pure "get"
      "put" -> pure "put"
      "post" -> pure "post"
      "delete" -> pure "delete"
      "options" -> pure "options"
      "head" -> pure "trace"
      "patch" -> pure "patch"
      "trace" -> pure "trace"
      _ -> codeGenError ("Unsupported operation method: " <> show method)

beelineMake :: HC.FromCode c => c
beelineMake =
  beelineRoutingVar "make"

beelineRouter :: HC.FromCode c => c
beelineRouter =
  beelineRoutingType "Router"

beelinePiece :: (HC.FromCode c, HC.ToCode c) => c
beelinePiece =
  beelineRoutingOperator "/-"

beelineParam :: (HC.FromCode c, HC.ToCode c) => c
beelineParam =
  beelineRoutingOperator "/+"

beelineMkParam :: HC.FromCode c => c
beelineMkParam =
  beelineRoutingConstructor "Param"

beelineParamDef :: HC.FromCode c => c
beelineParamDef =
  beelineRoutingType "ParameterDefinition"

beelineCoerceParam :: HC.FromCode c => c
beelineCoerceParam =
  beelineParamsVar "coerceParam"

beelineTextParam :: HC.FromCode c => c
beelineTextParam =
  beelineParamsVar "textParam"

beelineBooleanParam :: HC.FromCode c => c
beelineBooleanParam =
  beelineParamsVar "booleanParam"

beelineEnumParam :: (HC.FromCode c, Semigroup c) => HC.VarName -> c
beelineEnumParam toTextName =
  beelineParamsVar "boundedEnumParam"
    <> HC.fromText " "
    <> HC.varNameToCodeDefaultQualification toTextName

beelineIntegerParam :: HC.FromCode c => c
beelineIntegerParam =
  beelineParamsVar "integerParam"

beelineIntParam :: HC.FromCode c => c
beelineIntParam =
  beelineParamsVar "intParam"

beelineInt8Param :: HC.FromCode c => c
beelineInt8Param =
  beelineParamsVar "int8Param"

beelineInt16Param :: HC.FromCode c => c
beelineInt16Param =
  beelineParamsVar "int16Param"

beelineInt32Param :: HC.FromCode c => c
beelineInt32Param =
  beelineParamsVar "int32Param"

beelineInt64Param :: HC.FromCode c => c
beelineInt64Param =
  beelineParamsVar "int64Param"

beelineScientificParam :: HC.FromCode c => c
beelineScientificParam =
  beelineParamsVar "scientificParam"

beelineDoubleParam :: HC.FromCode c => c
beelineDoubleParam =
  beelineParamsVar "doubleParam"

beelineFloatParam :: HC.FromCode c => c
beelineFloatParam =
  beelineRoutingVar "floatParam"

beelineRoutingVar :: HC.FromCode c => T.Text -> c
beelineRoutingVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Beeline.Routing" (Just "R")

beelineRoutingConstructor :: HC.FromCode c => T.Text -> c
beelineRoutingConstructor =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Beeline.Routing" (Just "R")

beelineRoutingType :: HC.FromCode c => T.Text -> c
beelineRoutingType =
  HC.fromCode
    . HC.typeNameToCodeDefaultQualification
    . HC.toTypeName "Beeline.Routing" (Just "R")

beelineRoutingOperator :: (HC.FromCode c, HC.ToCode c) => T.Text -> c
beelineRoutingOperator op =
  HC.addReferences
    [HC.VarReference "Beeline.Routing" Nothing ("(" <> op <> ")")]
    (HC.fromText op)

beelineParameterSchema :: HC.FromCode c => c
beelineParameterSchema =
  beelineParamsType "ParameterSchema"

beelineQuerySchema :: HC.FromCode c => c
beelineQuerySchema =
  beelineParamsType "QuerySchema"

beelineHeaderSchema :: HC.FromCode c => c
beelineHeaderSchema =
  beelineParamsType "HeaderSchema"

beelineMakeParams :: HC.FromCode c => c
beelineMakeParams =
  beelineParamsVar "makeParams"

beelineQueryParam :: (HC.FromCode c, HC.ToCode c) => c
beelineQueryParam =
  beelineParamsOperator "?+"

beelineRequired :: HC.FromCode c => c
beelineRequired =
  beelineParamsVar "required"

beelineOptional :: HC.FromCode c => c
beelineOptional =
  beelineParamsVar "optional"

beelineExplodedArray :: HC.FromCode c => c
beelineExplodedArray =
  beelineParamsVar "explodedArray"

beelineExplodedNonEmpty :: HC.FromCode c => c
beelineExplodedNonEmpty =
  beelineParamsVar "explodedNonEmpty"

beelineCookies :: HC.FromCode c => c
beelineCookies =
  beelineParamsVar "cookies"

beelineOperation :: HC.FromCode c => c
beelineOperation =
  beelineHTTPType "Operation"

beelineDefaultOperation :: HC.FromCode c => c
beelineDefaultOperation =
  beelineHTTPVar "defaultOperation"

beelineRequestRoute :: HC.FromCode c => c
beelineRequestRoute =
  beelineHTTPVar "requestRoute"

beelineRequestQuerySchema :: HC.FromCode c => c
beelineRequestQuerySchema =
  beelineHTTPVar "requestQuerySchema"

beelineRequestHeaderSchema :: HC.FromCode c => c
beelineRequestHeaderSchema =
  beelineHTTPVar "requestHeaderSchema"

beelineResponseSchemas :: HC.FromCode c => c
beelineResponseSchemas =
  beelineHTTPVar "responseSchemas"

beelineRequestBodySchema :: HC.FromCode c => c
beelineRequestBodySchema =
  beelineHTTPVar "requestBodySchema"

beelineRequestBody :: HC.FromCode c => c
beelineRequestBody =
  beelineHTTPVar "requestBody"

beelineResponseBody :: HC.FromCode c => c
beelineResponseBody =
  beelineHTTPVar "responseBody"

beelineStatus :: HC.FromCode c => c
beelineStatus =
  beelineHTTPConstructor "Status"

beelineAnyStatus :: HC.FromCode c => c
beelineAnyStatus =
  beelineHTTPConstructor "AnyStatus"

beelineStatusRange :: HC.FromCode c => c
beelineStatusRange =
  beelineHTTPType "StatusRange"

beelineResponseBodySchema :: HC.FromCode c => c
beelineResponseBodySchema =
  beelineHTTPType "ResponseBodySchema"

beelineNoPathParamsType :: HC.FromCode c => c
beelineNoPathParamsType =
  beelineHTTPType "NoPathParams"

beelineNoQueryParams :: HC.FromCode c => c
beelineNoQueryParams =
  beelineHTTPType "NoQueryParams"

beelineNoHeaderParams :: HC.FromCode c => c
beelineNoHeaderParams =
  beelineHTTPType "NoHeaderParams"

beelineNoRequestBodyType :: HC.FromCode c => c
beelineNoRequestBodyType =
  beelineHTTPType "NoRequestBody"

beelineNoResponseBodyType :: HC.FromCode c => c
beelineNoResponseBodyType =
  beelineHTTPType "NoResponseBody"

beelineNoResponseBody :: HC.FromCode c => c
beelineNoResponseBody =
  beelineHTTPVar "noResponseBody"

beelineHTTPVar :: HC.FromCode c => T.Text -> c
beelineHTTPVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Beeline.HTTP.Client" (Just "H")

beelineHTTPConstructor :: HC.FromCode c => T.Text -> c
beelineHTTPConstructor =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Beeline.HTTP.Client" (Just "H")

beelineHTTPType :: HC.FromCode c => T.Text -> c
beelineHTTPType =
  HC.fromCode
    . HC.typeNameToCodeDefaultQualification
    . HC.toTypeName "Beeline.HTTP.Client" (Just "H")

beelineParamsVar :: HC.FromCode c => T.Text -> c
beelineParamsVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Beeline.Params" (Just "P")

beelineParamsType :: HC.FromCode c => T.Text -> c
beelineParamsType =
  HC.fromCode
    . HC.typeNameToCodeDefaultQualification
    . HC.toTypeName "Beeline.Params" (Just "P")

beelineParamsOperator :: (HC.FromCode c, HC.ToCode c) => T.Text -> c
beelineParamsOperator op =
  HC.addReferences
    [HC.VarReference "Beeline.Params" Nothing ("(" <> op <> ")")]
    (HC.fromText op)

toTextFunctionVarName :: HC.ModuleName -> Maybe T.Text -> HC.TypeName -> HC.VarName
toTextFunctionVarName moduleName mbQualifier typeName =
  HC.toVarName
    moduleName
    mbQualifier
    (HC.typeNameText typeName <> "ToText")
