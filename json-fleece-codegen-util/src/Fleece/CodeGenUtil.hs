{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil
  ( generateFleeceCode
  , CodeGenOptions
    ( CodeGenOptions
    , moduleBaseName
    )
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
  , OperationParamFormat (..)
  , OperationParamLocation (..)
  , CodeGenDataFormat (..)
  , CodeGenObjectField (..)
  , CodeGenObjectFieldType (..)
  , TypeMap
  , SchemaTypeInfo (..)
  , CodeSection (Type, Operation)
  , inferSchemaInfoForInputName
  , inferTypeForInputName
  , arrayTypeInfo
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
  , scientificFormat
  , floatFormat
  , doubleFormat
  , dayFormat
  , utcTimeFormat
  , enumFormat
  , nullFormat
  , HC.HaskellCode
  , HC.renderLazyText
  , HC.renderText
  , Modules
  ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
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
  }

codeGenError :: String -> CodeGen a
codeGenError = lift . Left . CodeGenError

type TypeMap =
  Map.Map T.Text CodeGenItem

data CodeGenItem
  = CodeGenItemType CodeGenType
  | CodeGenItemOperation CodeGenOperation
  | CodeGenItemOperationParam CodeGenOperationParam

data CodeGenType = CodeGenType
  { codeGenTypeOriginalName :: T.Text
  , codeGenTypeName :: HC.TypeName
  , codeGenTypeSchemaInfo :: SchemaTypeInfo
  , codeGenTypeDescription :: Maybe T.Text
  , codeGenTypeDataFormat :: CodeGenDataFormat
  }

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
  , codeGenOperationRequestBody :: Maybe CodeGenType
  , codeGenOperationResponses :: Map.Map ResponseStatus (Maybe SchemaTypeInfo)
  }

data CodeGenOperationParam = CodeGenOperationParam
  { codeGenOperationParamName :: T.Text
  , codeGenOperationParamModuleName :: HC.ModuleName
  , codeGenOperationParamTypeName :: HC.TypeName
  , codeGenOperationParamDefName :: HC.VarName
  , codeGenOperationParamArity :: OperationParamArity
  , codeGenOperationParamFormat :: OperationParamFormat
  , codeGenOperationParamLocation :: OperationParamLocation
  }

data OperationPathPiece
  = PathLiteral T.Text
  | PathParamRef T.Text HC.TypeName HC.VarName

data OperationParamArity
  = ExactlyOne
  | AtMostOne
  | AtLeastZero
  | AtLeastOne
  deriving (Show, Eq)

data OperationParamFormat
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
  deriving (Show, Eq)

data OperationParamLocation
  = ParamLocationPath
  | ParamLocationQuery
  | ParamLocationHeader
  deriving (Show, Eq)

data CodeGenDataFormat
  = CodeGenNewType SchemaTypeInfo
  | CodeGenEnum [T.Text]
  | CodeGenObject [CodeGenObjectField]
  | CodeGenArray CodeGenObjectFieldType

data CodeGenObjectField = CodeGenObjectField
  { codeGenFieldName :: T.Text
  , codeGenFieldType :: CodeGenObjectFieldType
  , codeGenFieldRequired :: Bool
  }

data CodeGenObjectFieldType
  = TypeReference T.Text
  | CodeGenFieldArray
      Bool
      -- ^ whether the array itself is nullable
      CodeGenObjectFieldType

resolveFieldTypeInfo ::
  TypeMap ->
  CodeGenObjectFieldType ->
  CodeGen SchemaTypeInfo
resolveFieldTypeInfo typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          case Map.lookup ref typeMap of
            Just (CodeGenItemType codeGenType) ->
              pure (codeGenTypeSchemaInfo codeGenType)
            _ ->
              codeGenError $ "Type " <> show ref <> " not found."
        CodeGenFieldArray nullable itemType ->
          let
            modifier =
              if nullable
                then nullableTypeInfo
                else id
          in
            fmap (modifier . arrayTypeInfo) (go itemType)
  in
    go

resolveFieldDescription ::
  TypeMap ->
  CodeGenObjectFieldType ->
  Maybe T.Text
resolveFieldDescription typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          case Map.lookup ref typeMap of
            Just (CodeGenItemType codeGenType) ->
              codeGenTypeDescription codeGenType
            Just (CodeGenItemOperation _operation) ->
              Nothing
            Just (CodeGenItemOperationParam _param) ->
              Nothing
            Nothing ->
              Nothing
        CodeGenFieldArray _nullable itemType ->
          go itemType
  in
    go

dayFormat :: CodeGenDataFormat
dayFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "Day")
      (fleeceCoreVar "day")

utcTimeFormat :: CodeGenDataFormat
utcTimeFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "UTCTime")
      (fleeceCoreVar "utcTime")

textFormat :: CodeGenDataFormat
textFormat =
  CodeGenNewType textSchemaTypeInfo

textSchemaTypeInfo :: SchemaTypeInfo
textSchemaTypeInfo =
  primitiveSchemaTypeInfo textType (fleeceCoreVar "text")

floatFormat :: CodeGenDataFormat
floatFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo floatType (fleeceCoreVar "float")

doubleFormat :: CodeGenDataFormat
doubleFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo doubleType (fleeceCoreVar "double")

scientificFormat :: CodeGenDataFormat
scientificFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo scientificType (fleeceCoreVar "number")

int32Format :: CodeGenDataFormat
int32Format =
  CodeGenNewType int32SchemaTypeInfo

int32SchemaTypeInfo :: SchemaTypeInfo
int32SchemaTypeInfo =
  primitiveSchemaTypeInfo int32Type (fleeceCoreVar "int32")

int64Format :: CodeGenDataFormat
int64Format =
  CodeGenNewType int64SchemaTypeInfo

int64SchemaTypeInfo :: SchemaTypeInfo
int64SchemaTypeInfo =
  primitiveSchemaTypeInfo int64Type (fleeceCoreVar "int64")

integerFormat :: CodeGenDataFormat
integerFormat =
  CodeGenNewType integerSchemaTypeInfo

integerSchemaTypeInfo :: SchemaTypeInfo
integerSchemaTypeInfo =
  primitiveSchemaTypeInfo integerType (fleeceCoreVar "integer")

boolFormat :: CodeGenDataFormat
boolFormat =
  CodeGenNewType boolSchemaTypeInfo

boolSchemaTypeInfo :: SchemaTypeInfo
boolSchemaTypeInfo =
  primitiveSchemaTypeInfo boolType (fleeceCoreVar "boolean")

enumFormat :: [T.Text] -> CodeGenDataFormat
enumFormat =
  CodeGenEnum

nullFormat :: CodeGenDataFormat
nullFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo (fleeceCoreType "Null") (fleeceCoreVar "null")

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
    }

generateFleeceCode :: TypeMap -> CodeGen Modules
generateFleeceCode typeMap =
  traverse (generateItem typeMap) (Map.elems typeMap)

generateItem ::
  TypeMap ->
  CodeGenItem ->
  CodeGen (FilePath, HC.HaskellCode)
generateItem typeMap codeGenItem =
  case codeGenItem of
    CodeGenItemType codeGenType ->
      generateSchemaCode typeMap codeGenType
    CodeGenItemOperation codeGenOperation ->
      generateOperationCode typeMap codeGenOperation
    CodeGenItemOperationParam codeGenOperationParam ->
      generateOperationParamCode codeGenOperationParam

generateOperationCode ::
  TypeMap ->
  CodeGenOperation ->
  CodeGen (FilePath, HC.HaskellCode)
generateOperationCode _typeMap codeGenOperation = do
  moduleName <-
    generatedModuleName
      Operation
      (codeGenOperationOriginalName codeGenOperation)

  beelineMethod <-
    methodToBeelineFunction $
      codeGenOperationMethod codeGenOperation

  let
    filePath =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    header =
      operationModuleHeader
        moduleName
        mbPathParamsTypeName
        mbQueryParamsTypeName
        mbQueryParamsSchemaName

    urlPath =
      codeGenOperationPath codeGenOperation

    mbPathParamsTypeName =
      case routeFields of
        [] -> Nothing
        _ -> Just (HC.toTypeName moduleName Nothing "PathParams")

    pathParamsTypeNameAsCode =
      case mbPathParamsTypeName of
        Nothing -> beelineNoPathParamsType
        Just name -> HC.typeNameToCode Nothing name

    mbRequestBodyTypeName =
      fmap
        codeGenTypeName
        (codeGenOperationRequestBody codeGenOperation)

    mkRequestBody requestBodyTypeName =
      let
        requestBodySchemaName =
          HC.varNameToCodeDefaultQualification
            . fleeceSchemaNameForType
            $ requestBodyTypeName
      in
        beelineRequestBodySchema
          <> " = "
          <> beelineRequestBody
          <> " "
          <> fleeceJSON
          <> " "
          <> requestBodySchemaName

    operationType =
      HC.lines . (HC.indent 2 beelineOperation :) $
        fmap (HC.indent 4) $
          [ beelineContentTypeDecodingError
          , pathParamsTypeNameAsCode
          , queryParamsTypeNameAsCode
          , maybe
              beelineNoRequestBodyType
              HC.typeNameToCodeDefaultQualification
              mbRequestBodyTypeName
          , HC.typeNameToCode Nothing responsesTypeName
          ]

    operationFields =
      HC.delimitLines "{ " ", " $
        catMaybes
          [ Just (beelineRequestRoute <> " = route")
          , fmap (\name -> beelineRequestQuerySchema <> " = " <> name) mbQueryParamsSchemaName
          , fmap mkRequestBody mbRequestBodyTypeName
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

    mbPathParamsDeclaration =
      fmap (\name -> HC.record name routeFields) mbPathParamsTypeName

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

    queryParams =
      filter (\p -> codeGenOperationParamLocation p == ParamLocationQuery)
        . codeGenOperationParams
        $ codeGenOperation

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
            HC.typeNameToCodeDefaultQualification nonEmptyType
              <> " "
              <> paramTypeName

    mkParamField param =
      ( HC.toVarName moduleName Nothing (codeGenOperationParamName param)
      , paramFieldType param
      , Nothing
      )

    queryParamFields =
      map mkParamField queryParams

    mbQueryParamsTypeName =
      case queryParamFields of
        [] -> Nothing
        _ -> Just (HC.toTypeName moduleName Nothing "QueryParams")

    queryParamsTypeNameAsCode =
      case mbQueryParamsTypeName of
        Nothing -> beelineNoQueryParams
        Just name -> HC.typeNameToCode Nothing name

    mbQueryParamsDeclaration =
      fmap (\name -> HC.record name queryParamFields) mbQueryParamsTypeName

    queryParamsSchemaType =
      beelineQuerySchema
        <> " q => q "
        <> queryParamsTypeNameAsCode
        <> " "
        <> queryParamsTypeNameAsCode

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
      in
        beelineQueryParam
          <> " "
          <> beelineArity
          <> " "
          <> paramFieldName
          <> " "
          <> HC.varNameToCodeDefaultQualification (codeGenOperationParamDefName param)

    mbQueryParamsSchemaName =
      case mbQueryParamsTypeName of
        Nothing -> Nothing
        Just _ -> Just (HC.fromText "queryParamsSchema")

    mbQueryParamsSchema =
      case mbQueryParamsSchemaName of
        Nothing -> Nothing
        Just schemaName ->
          Just $
            HC.lines
              ( schemaName <> " :: " <> queryParamsSchemaType
                  : schemaName <> " ="
                  : HC.indent 2 (beelineMakeQuery <> " " <> queryParamsTypeNameAsCode)
                  : map (HC.indent 4 . mkParamSchema) queryParams
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

    responses =
      let
        statusAndSchema =
          Map.toList
            . codeGenOperationResponses
            $ codeGenOperation
        addResponseType (status, schema) = (responsesTypeName, status, schema)
      in
        map addResponseType statusAndSchema

    responsesTypeName =
      HC.toTypeName moduleName Nothing "Responses"

    responsesType =
      HC.sumType responsesTypeName (map mkResponseConstructor responses)

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
            <> beelineContentTypeDecodingError
            <> " "
            <> HC.typeNameToCode Nothing responsesTypeName
            <> ")]"
            : "responseSchemas ="
            : map (HC.indent 2) (responseSchemaLines <> ["]"])
        )

    moduleBody =
      HC.declarations . catMaybes $
        [ Just operation
        , mbPathParamsDeclaration
        , Just route
        , mbQueryParamsDeclaration
        , mbQueryParamsSchema
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

operationModuleHeader ::
  HC.ModuleName ->
  Maybe HC.TypeName ->
  Maybe HC.TypeName ->
  Maybe HC.HaskellCode ->
  HC.HaskellCode
operationModuleHeader moduleName mbPathParamsTypeName mbQueryParamsTypeName mbQueryParamsSchemaName =
  let
    exports =
      HC.delimitLines "( " ", " . catMaybes $
        [ Just "operation"
        , fmap (\name -> HC.typeNameToCode Nothing name <> "(..)") mbPathParamsTypeName
        , Just "route"
        , fmap (\name -> HC.typeNameToCode Nothing name <> "(..)") mbQueryParamsTypeName
        , mbQueryParamsSchemaName
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
  CodeGen (FilePath, HC.HaskellCode)
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

    paramFormat =
      codeGenOperationParamFormat codeGenOperationParam

    pragmas =
      HC.lines
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]

    typeDeclaration =
      if HC.typeNameModule typeName == moduleName
        then case paramFormatToHaskellType paramFormat of
          Right haskellType ->
            Just $
              HC.newtype_
                typeName
                (HC.fromCode (HC.typeNameToCodeDefaultQualification haskellType))
          Left enumValues ->
            Just . snd $ generateEnum typeName enumValues
        else Nothing

    beelineBaseDef =
      paramFormatToBeelineType
        moduleName
        typeName
        paramFormat

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
      operationParamHeader moduleName typeName defName

    moduleBody =
      HC.declarations . catMaybes $
        [ typeDeclaration
        , Just defDeclaration
        ]

    code =
      HC.declarations $
        [ pragmas
        , header
        , importDeclarations moduleName moduleBody
        , moduleBody
        ]

  pure (filePath, code)

paramFormatToHaskellType :: OperationParamFormat -> Either [T.Text] HC.TypeName
paramFormatToHaskellType format =
  case format of
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

paramFormatToBeelineType ::
  (HC.FromCode c, Semigroup c) =>
  HC.ModuleName ->
  HC.TypeName ->
  OperationParamFormat ->
  c
paramFormatToBeelineType moduleName typeName format =
  case format of
    ParamTypeString -> beelineTextParam
    ParamTypeBoolean -> beelineBooleanParam
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
          HC.toVarName
            typeModuleName
            qualifier
            (typeNameText <> "ToText")
      in
        beelineEnumParam toTextName
    ParamTypeInteger -> beelineIntegerParam
    ParamTypeInt -> beelineIntParam
    ParamTypeInt8 -> beelineInt8Param
    ParamTypeInt16 -> beelineInt16Param
    ParamTypeInt32 -> beelineInt32Param
    ParamTypeInt64 -> beelineInt64Param
    ParamTypeScientific -> beelineScientificParam
    ParamTypeDouble -> beelineDoubleParam
    ParamTypeFloat -> beelineFloatParam

operationParamHeader :: HC.ModuleName -> HC.TypeName -> HC.VarName -> HC.HaskellCode
operationParamHeader moduleName typeName paramDef =
  let
    typeExport =
      if moduleName == HC.typeNameModule typeName
        then Just (HC.typeNameToCode Nothing typeName <> "(..)")
        else Nothing

    paramExport =
      HC.varNameToCode Nothing paramDef

    exportLines =
      HC.delimitLines "( " ", " $
        catMaybes
          [ typeExport
          , Just paramExport
          ]
  in
    HC.lines
      ( "module " <> HC.toCode moduleName
          : map (HC.indent 2) (exportLines <> [") where"])
      )

generateSchemaCode ::
  TypeMap ->
  CodeGenType ->
  CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode typeMap codeGenType = do
  (moduleName, typeName) <-
    inferTypeForInputName Type (codeGenTypeOriginalName codeGenType)

  let
    path =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    format =
      codeGenTypeDataFormat codeGenType

  (extraExports, moduleBody) <-
    case format of
      CodeGenNewType baseTypeInfo ->
        pure $
          generateFleeceNewtype
            typeName
            (schemaTypeExpr baseTypeInfo)
            (schemaTypeSchema baseTypeInfo)
      CodeGenEnum values ->
        pure $ generateFleeceEnum typeName values
      CodeGenObject fields ->
        generateFleeceObject typeMap typeName fields
      CodeGenArray itemType ->
        generateFleeceArray typeMap typeName itemType

  let
    header =
      schemaTypeModuleHeader moduleName typeName extraExports

    code =
      HC.declarations $
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
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
  HC.TypeName ->
  HC.TypeExpression ->
  HC.HaskellCode ->
  ([HC.VarName], HC.HaskellCode)
generateFleeceNewtype wrapperName baseType schemaName =
  let
    newtypeDecl =
      HC.newtype_ wrapperName baseType

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
  in
    (extraExports, body)

generateFleeceEnum ::
  HC.TypeName ->
  [T.Text] ->
  ([HC.VarName], HC.HaskellCode)
generateFleeceEnum typeName enumValues =
  let
    (toTextName, enum) =
      generateEnum typeName enumValues

    fleeceSchema =
      fleeceSchemaForType typeName $
        [ fleeceCoreVar "boundedEnum" <> " " <> HC.varNameToCode Nothing toTextName
        ]
  in
    ([toTextName], HC.declarations [enum, fleeceSchema])

generateFleeceObject ::
  TypeMap ->
  HC.TypeName ->
  [CodeGenObjectField] ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceObject typeMap typeName codeGenFields = do
  let
    moduleName =
      HC.typeNameModule typeName

  fields <-
    traverse (mkFleeceSchemaField typeMap moduleName) codeGenFields

  let
    fieldNameAndType field =
      (fieldName field, fieldTypeName field, fieldDescription field)

    recordDecl =
      HC.record typeName (map fieldNameAndType fields)

    fleeceField field =
      HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#+)"] $
        "#+ "
          <> fleeceFieldFunction field
          <> " "
          <> HC.stringLiteral (fieldJSONName field)
          <> " "
          <> HC.varNameToCode Nothing (fieldName field)
          <> " "
          <> fieldFleeceSchemaCode field

    fleeceSchema =
      fleeceSchemaForType typeName $
        ( fleeceCoreVar "object" <> " " <> HC.dollar
            : "  " <> fleeceCoreVar "constructor" <> " " <> HC.typeNameToCode Nothing typeName
            : map (HC.indent 4 . fleeceField) fields
        )

    extraExports =
      []

    body =
      HC.declarations
        [ recordDecl
        , fleeceSchema
        ]

  pure $ (extraExports, body)

generateFleeceArray ::
  TypeMap ->
  HC.TypeName ->
  CodeGenObjectFieldType ->
  CodeGen ([HC.VarName], HC.HaskellCode)
generateFleeceArray typeMap typeName itemType = do
  typeInfo <- fmap arrayTypeInfo (resolveFieldTypeInfo typeMap itemType)
  pure $
    generateFleeceNewtype
      typeName
      (schemaTypeExpr typeInfo)
      (schemaTypeSchema typeInfo)

data FleeceSchemaField = FleeceSchemaField
  { fieldName :: HC.VarName
  , fieldJSONName :: T.Text
  , fieldCodeGenField :: CodeGenObjectField
  , fieldBaseSchemaTypeInfo :: SchemaTypeInfo
  , fieldDescription :: Maybe T.Text
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

fleeceFieldFunction :: FleeceSchemaField -> HC.HaskellCode
fleeceFieldFunction field =
  if fieldRequired field
    then fleeceCoreVar "required"
    else fleeceCoreVar "optional"

mkFleeceSchemaField ::
  TypeMap ->
  HC.ModuleName ->
  CodeGenObjectField ->
  CodeGen FleeceSchemaField
mkFleeceSchemaField typeMap moduleName codeGenField = do
  let
    name =
      codeGenFieldName codeGenField

    fieldType =
      codeGenFieldType codeGenField

  typeInfo <- resolveFieldTypeInfo typeMap fieldType

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
  }

primitiveSchemaTypeInfo :: HC.TypeName -> HC.HaskellCode -> SchemaTypeInfo
primitiveSchemaTypeInfo typeName schema =
  SchemaTypeInfo
    { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
    , schemaTypeSchema = schema
    }

inferSchemaInfoForInputName :: T.Text -> CodeGen SchemaTypeInfo
inferSchemaInfoForInputName name = do
  (_moduleName, typeName) <- inferTypeForInputName Type name

  let
    schema =
      fleeceSchemaNameForType typeName

  pure $
    SchemaTypeInfo
      { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
      , schemaTypeSchema = HC.varNameToCodeDefaultQualification schema
      }

inferTypeForInputName :: CodeSection -> T.Text -> (CodeGen (HC.ModuleName, HC.TypeName))
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

generateEnum ::
  HC.TypeName ->
  [T.Text] ->
  (HC.VarName, HC.HaskellCode)
generateEnum typeName enumValues =
  let
    mkEnumItem t =
      (t, HC.toConstructorName typeName t)

    enumItems =
      map mkEnumItem enumValues

    moduleName =
      HC.typeNameModule typeName

    enumDeclaration =
      HC.enum typeName (map snd enumItems)

    toTextName =
      HC.toVarName
        moduleName
        Nothing
        (HC.typeNameText typeName <> "ToText")

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

nonEmptyType :: HC.TypeName
nonEmptyType =
  HC.toTypeName "Data.List.NonEmpty" (Just "NEL") "NonEmpty"

fleeceClass :: HC.TypeName
fleeceClass =
  fleeceCoreType "Fleece"

fleeceCoreType :: T.Text -> HC.TypeName
fleeceCoreType =
  HC.toTypeName "Fleece.Core" (Just "FC")

fleeceCoreVar :: HC.FromCode c => T.Text -> c
fleeceCoreVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Fleece.Core" (Just "FC")

fleeceJSON :: HC.FromCode c => c
fleeceJSON =
  fleeceAesonBeelineConstructor "JSON"

fleeceAesonBeelineConstructor :: HC.FromCode c => T.Text -> c
fleeceAesonBeelineConstructor =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Fleece.Aeson.Beeline" (Just "FA")

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
  beelineRoutingVar "coerceParam"

beelineTextParam :: HC.FromCode c => c
beelineTextParam =
  beelineRoutingVar "textParam"

beelineBooleanParam :: HC.FromCode c => c
beelineBooleanParam =
  beelineRoutingVar "booleanParam"

beelineEnumParam :: (HC.FromCode c, Semigroup c) => HC.VarName -> c
beelineEnumParam toTextName =
  beelineRoutingVar "boundedEnumParam"
    <> HC.fromText " "
    <> HC.varNameToCodeDefaultQualification toTextName

beelineIntegerParam :: HC.FromCode c => c
beelineIntegerParam =
  beelineRoutingVar "integerParam"

beelineIntParam :: HC.FromCode c => c
beelineIntParam =
  beelineRoutingVar "intParam"

beelineInt8Param :: HC.FromCode c => c
beelineInt8Param =
  beelineRoutingVar "int8Param"

beelineInt16Param :: HC.FromCode c => c
beelineInt16Param =
  beelineRoutingVar "int16Param"

beelineInt32Param :: HC.FromCode c => c
beelineInt32Param =
  beelineRoutingVar "int32Param"

beelineInt64Param :: HC.FromCode c => c
beelineInt64Param =
  beelineRoutingVar "int32Param"

beelineScientificParam :: HC.FromCode c => c
beelineScientificParam =
  beelineRoutingVar "scientificParam"

beelineDoubleParam :: HC.FromCode c => c
beelineDoubleParam =
  beelineRoutingVar "doubleParam"

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

beelineQuerySchema :: HC.FromCode c => c
beelineQuerySchema =
  beelineHTTPType "QuerySchema"

beelineMakeQuery :: HC.FromCode c => c
beelineMakeQuery =
  beelineHTTPVar "makeQuery"

beelineQueryParam :: (HC.FromCode c, HC.ToCode c) => c
beelineQueryParam =
  beelineHTTPOperator "?+"

beelineRequired :: HC.FromCode c => c
beelineRequired =
  beelineHTTPVar "required"

beelineOptional :: HC.FromCode c => c
beelineOptional =
  beelineHTTPVar "optional"

beelineExplodedArray :: HC.FromCode c => c
beelineExplodedArray =
  beelineHTTPVar "explodedArray"

beelineExplodedNonEmpty :: HC.FromCode c => c
beelineExplodedNonEmpty =
  beelineHTTPVar "explodedNonEmpty"

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

beelineContentTypeDecodingError :: HC.FromCode c => c
beelineContentTypeDecodingError =
  beelineHTTPConstructor "ContentTypeDecodingError"

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

beelineHTTPOperator :: (HC.FromCode c, HC.ToCode c) => T.Text -> c
beelineHTTPOperator op =
  HC.addReferences
    [HC.VarReference "Beeline.HTTP.Client" Nothing ("(" <> op <> ")")]
    (HC.fromText op)
