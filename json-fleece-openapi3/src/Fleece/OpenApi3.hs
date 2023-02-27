{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateFleeceCode
  , CodeGenOptions
    ( CodeGenOptions
    , moduleBaseName
    )
  , CodeGenResult
  , CodeGenError
    ( UnableToDetermineTypeNameForSchema
    )
  , HC.HaskellCode
  , HC.renderLazyText
  , HC.renderText
  , Modules
  ) where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OA
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified HaskellCode as HC

data CodeGenError
  = UnableToDetermineTypeNameForSchema OA.Schema
  | UnsupportedItem String OA.Schema
  | UnsupportedEnumValue Aeson.Value
  | ReferencedSchemaMissing OA.Reference
  deriving (Show)

data CodeGenContext = CodeGenContext
  { codeGenApi :: OA.OpenApi
  , codeGenOptions :: CodeGenOptions
  }

type CodeGen = ReaderT CodeGenContext (Either CodeGenError)
type CodeGenResult = Either CodeGenError

type Modules =
  [(FilePath, HC.HaskellCode)]

data CodeGenOptions = CodeGenOptions
  { moduleBaseName :: T.Text
  }

codeGenError :: CodeGenError -> CodeGen a
codeGenError = lift . Left

getSchemaDefinitions :: CodeGen (OA.Definitions OA.Schema)
getSchemaDefinitions =
  asks (OA._componentsSchemas . OA._openApiComponents . codeGenApi)

getReferencedSchema :: OA.Referenced OA.Schema -> CodeGen OA.Schema
getReferencedSchema ref =
  case ref of
    OA.Ref reference -> do
      schemas <- getSchemaDefinitions
      case IOHM.lookup (OA.getReference reference) schemas of
        Nothing -> codeGenError (ReferencedSchemaMissing reference)
        Just schema -> pure schema
    OA.Inline schema ->
      pure schema

generateFleeceCode :: CodeGenOptions -> OA.OpenApi -> CodeGenResult Modules
generateFleeceCode options api =
  let
    codeGen :: CodeGen Modules
    codeGen = do
      schemas <- getSchemaDefinitions
      traverse (uncurry generateSchemaCode) (IOHM.toList schemas)

    context =
      CodeGenContext
        { codeGenApi = api
        , codeGenOptions = options
        }
  in
    runReaderT codeGen context

generateSchemaCode :: T.Text -> OA.Schema -> CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode schemaKey schema = do
  options <- asks codeGenOptions

  let
    typeName =
      HC.toTypeName schemaKey

    moduleNameText =
      moduleBaseName options <> "." <> HC.renderText typeName

    moduleName =
      HC.ModuleName moduleNameText

    path =
      T.unpack (T.replace "." "/" moduleNameText <> ".hs")

  moduleBody <-
    case OA._schemaType schema of
      Just OA.OpenApiString -> generateHaskellString typeName schema
      Just OA.OpenApiNumber -> pure . HC.toCode $ typeName
      Just OA.OpenApiInteger -> pure . HC.toCode $ typeName
      Just OA.OpenApiBoolean -> pure . HC.toCode $ typeName
      Just OA.OpenApiArray -> pure . HC.toCode $ typeName
      Just OA.OpenApiObject -> generateHaskellObject typeName schema
      Just OA.OpenApiNull -> pure . HC.toCode $ typeName
      Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)

  header <- moduleHeader moduleName typeName

  let
    code =
      HC.declarations $
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , header
        , importDeclarations options moduleName moduleBody
        , (moduleBody)
        ]

  pure (path, code)

moduleHeader :: HC.ModuleName -> HC.TypeName -> CodeGen HC.HaskellCode
moduleHeader moduleName typeName = do
  schemaName <- fleeceSchemaNameForType typeName
  pure $
    HC.lines
      [ "module " <> HC.toCode moduleName
      , HC.indent 2 ("( " <> HC.toCode typeName <> "(..)")
      , HC.indent 2 (", " <> HC.toCode schemaName)
      , HC.indent 2 ") where"
      ]

importDeclarations ::
  CodeGenOptions ->
  HC.ModuleName ->
  HC.HaskellCode ->
  HC.HaskellCode
importDeclarations options thisModuleName code =
  let
    mkImport :: HC.ExternalReference -> Map.Map HC.ModuleName (Set.Set T.Text)
    mkImport ref =
      case ref of
        HC.TypeReference typeName ->
          let
            moduleName =
              HC.ModuleName $
                case typeName of
                  "Text" -> "Data.Text"
                  "Scientific" -> "Data.Scientific"
                  "Bool" -> "Prelude"
                  "Integer" -> "Prelude"
                  "Show" -> "Prelude"
                  "Eq" -> "Prelude"
                  "Ord" -> "Prelude"
                  "Enum" -> "Prelude"
                  "Bounded" -> "Prelude"
                  "Maybe" -> "Prelude"
                  "Either" -> "Prelude"
                  _ -> moduleBaseName options <> "." <> typeName
          in
            if moduleName == thisModuleName
              then Map.empty
              else Map.singleton moduleName (Set.singleton typeName)
        HC.VarReference moduleName varName ->
          if moduleName == thisModuleName
            then Map.empty
            else Map.singleton moduleName (Set.singleton varName)

    imports =
      Map.unionsWith
        Set.union
        (map mkImport . Set.toList . HC.references $ code)

    mkImportLine (moduleName, varSet) =
      "import "
        <> HC.toCode moduleName
        <> " ("
        <> HC.intercalate ", " (map HC.fromText (Set.toList varSet))
        <> ")"

    importLines =
      map mkImportLine (Map.toList imports)
  in
    HC.lines
      ( "import qualified Fleece.Core as FC"
          : importLines
      )

generateHaskellString :: HC.TypeName -> OA.Schema -> CodeGen HC.HaskellCode
generateHaskellString typeName schema = do
  case OA._schemaEnum schema of
    Just enumValues -> generateHaskellEnum typeName enumValues
    Nothing -> do
      let
        newtypeDecl =
          HC.lines
            [ "newtype " <> HC.toCode typeName <> " Text"
            , HC.indent 2 (HC.deriving_ ["Show", "Eq"])
            ]

      fleeceSchema <-
        fleeceSchemaForType typeName $
          [ "FC.coerceSchema FC.text"
          ]

      pure $
        HC.declarations
          [ newtypeDecl
          , fleeceSchema
          ]

generateHaskellEnum :: HC.TypeName -> [Aeson.Value] -> CodeGen HC.HaskellCode
generateHaskellEnum typeName enumValues = do
  let
    mkEnumItem value =
      case value of
        Aeson.String t -> pure (t, HC.toConstructorName t)
        _ -> codeGenError (UnsupportedEnumValue value)

  enumItems <- traverse mkEnumItem enumValues
  moduleName <- moduleNameForType typeName

  let
    enumDeclaration =
      HC.enum typeName (map snd enumItems)

    toTextName =
      HC.toVarName
        moduleName
        (HC.renderText typeName <> "ToText")

    toText =
      HC.lines
        ( HC.typeAnnotate toTextName (HC.toCode typeName <> " -> " <> HC.toCode textType)
            : HC.toCode toTextName <> " v ="
            : HC.indent 2 (HC.toCode (HC.toVarName "Data.Text" "pack") <> " " <> HC.dollar)
            : HC.indent 4 "case v of"
            : map (HC.indent 6 . mkToTextCase) enumItems
        )

    mkToTextCase (text, constructor) =
      HC.caseMatch constructor (HC.stringLiteral text)

  fleeceSchema <-
    fleeceSchemaForType typeName $
      [ "FC.boundedEnum " <> HC.toCode toTextName
      ]

  pure $
    HC.declarations
      [ enumDeclaration
      , toText
      , fleeceSchema
      ]

generateHaskellObject :: HC.TypeName -> OA.Schema -> CodeGen HC.HaskellCode
generateHaskellObject typeName schema = do
  moduleName <- moduleNameForType typeName

  fields <-
    traverse (uncurry $ mkField moduleName (OA._schemaRequired schema))
      . filter (\(name, _) -> not (name == "_links"))
      . IOHM.toList
      . OA._schemaProperties
      $ schema

  let
    fieldNameAndType field =
      (fieldName field, fieldTypeName field, fieldDescription field)

    recordDecl =
      HC.record typeName (map fieldNameAndType fields)

    fleeceField field =
      HC.addReferences [HC.VarReference "Fleece.Core" "(#+)"] $
        "#+ "
          <> fleeceFieldFunction field
          <> " "
          <> HC.stringLiteral (fieldJSONName field)
          <> " "
          <> HC.toCode (fieldName field)
          <> " "
          <> fieldFleeceSchemaCode field

  fleeceSchema <-
    fleeceSchemaForType typeName $
      ( "FC.object " <> HC.dollar
          : "  FC.constructor " <> HC.toCode typeName
          : map (HC.indent 4 . fleeceField) fields
      )

  pure $
    HC.declarations
      [ recordDecl
      , fleeceSchema
      ]

data Field = Field
  { fieldName :: HC.VarName
  , fieldBaseTypeName :: HC.TypeName
  , fieldJSONName :: OA.ParamName
  , fieldJSONSchema :: OA.Schema
  , fieldRequired :: Bool
  , fieldBaseFleeceSchemaName :: HC.HaskellCode
  , fieldDescription :: Maybe T.Text
  }

fieldTypeName :: Field -> HC.TypeName
fieldTypeName field =
  case (fieldRequired field, fieldNullable field) of
    (True, False) -> fieldBaseTypeName field
    (True, True) -> HC.eitherOf "FC.Null" (fieldBaseTypeName field)
    (False, False) -> HC.maybeOf (fieldBaseTypeName field)
    (False, True) ->
      HC.maybeOf $
        "(" <> HC.eitherOf "FC.Null" (fieldBaseTypeName field) <> ")"

fieldNullable :: Field -> Bool
fieldNullable field =
  case OA._schemaNullable (fieldJSONSchema field) of
    Nothing -> False
    Just b -> b

fieldFleeceSchemaCode :: Field -> HC.HaskellCode
fieldFleeceSchemaCode field =
  if fieldNullable field
    then "(FC.nullable " <> fieldBaseFleeceSchemaName field <> ")"
    else fieldBaseFleeceSchemaName field

fleeceFieldFunction :: Field -> HC.HaskellCode
fleeceFieldFunction field =
  if fieldRequired field
    then "FC.required"
    else "FC.optional"

mkField ::
  HC.ModuleName ->
  [OA.ParamName] ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CodeGen Field
mkField moduleName requiredParams paramName schemaRef = do
  typeName <- schemaRefTypeName schemaRef
  fieldSchema <- getReferencedSchema schemaRef
  fleeceSchemaName <- fleeceSchemaForReference schemaRef

  pure $
    Field
      { fieldName = HC.toVarName moduleName paramName
      , fieldBaseTypeName = typeName
      , fieldRequired = paramName `elem` requiredParams
      , fieldJSONName = paramName
      , fieldJSONSchema = fieldSchema
      , fieldBaseFleeceSchemaName = fleeceSchemaName
      , fieldDescription = OA._schemaDescription fieldSchema
      }

schemaRefTypeName :: OA.Referenced OA.Schema -> CodeGen HC.TypeName
schemaRefTypeName schemaRef =
  case schemaRef of
    OA.Ref ref -> pure . HC.toTypeName . OA.getReference $ ref
    OA.Inline schema -> schemaTypeName schema

schemaTypeName :: OA.Schema -> CodeGen HC.TypeName
schemaTypeName schema =
  case OA._schemaTitle schema of
    Just t -> pure . HC.toTypeName $ t
    Nothing ->
      case OA._schemaType schema of
        Just OA.OpenApiString -> pure textType
        Just OA.OpenApiNumber -> pure "Scientific"
        Just OA.OpenApiInteger -> pure "Integer"
        Just OA.OpenApiBoolean -> pure "Bool"
        Just OA.OpenApiArray ->
          case OA._schemaItems schema of
            Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)
            Just (OA.OpenApiItemsObject itemSchemaRef) -> do
              itemName <- schemaRefTypeName itemSchemaRef
              pure (HC.listOf itemName)
            Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
              codeGenError (UnsupportedItem "Heterogenous Array" schema)
        Just OA.OpenApiObject -> codeGenError (UnsupportedItem "Object" schema)
        Just OA.OpenApiNull -> codeGenError (UnsupportedItem "Null" schema)
        Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)

fleeceSchemaForReference :: OA.Referenced OA.Schema -> CodeGen HC.HaskellCode
fleeceSchemaForReference schemaRef =
  case schemaRef of
    OA.Ref ref ->
      fmap HC.toCode
        . fleeceSchemaNameForType
        . HC.toTypeName
        . OA.getReference
        $ ref
    OA.Inline schema ->
      fleeceSchemaForSchema schema

fleeceSchemaForSchema :: OA.Schema -> CodeGen HC.HaskellCode
fleeceSchemaForSchema schema =
  case OA._schemaTitle schema of
    Just t -> fmap HC.toCode . fleeceSchemaNameForType . HC.toTypeName $ t
    Nothing ->
      case OA._schemaType schema of
        Just OA.OpenApiString -> pure "FC.text"
        Just OA.OpenApiNumber -> pure "FC.number"
        Just OA.OpenApiInteger -> pure "FC.integer"
        Just OA.OpenApiBoolean -> pure "FC.boolean"
        Just OA.OpenApiArray ->
          case OA._schemaItems schema of
            Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)
            Just (OA.OpenApiItemsObject itemSchemaRef) -> do
              itemName <- fleeceSchemaForReference itemSchemaRef
              pure ("(FC.list " <> itemName <> ")")
            Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
              codeGenError (UnsupportedItem "Heterogenous Array" schema)
        Just OA.OpenApiObject -> codeGenError (UnsupportedItem "Object" schema)
        Just OA.OpenApiNull -> codeGenError (UnsupportedItem "Null" schema)
        Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)

fleeceSchemaNameForType :: HC.TypeName -> CodeGen HC.VarName
fleeceSchemaNameForType typeName = do
  moduleName <- moduleNameForType typeName
  pure $
    HC.toVarName
      moduleName
      (HC.renderText typeName <> "Schema")

fleeceSchemaForType :: HC.TypeName -> [HC.HaskellCode] -> CodeGen HC.HaskellCode
fleeceSchemaForType typeName bodyLines = do
  schemaName <- fleeceSchemaNameForType typeName

  let
    declType =
      HC.typeAnnotate schemaName ("FC.Fleece schema => schema " <> HC.toCode typeName)

    declImpl =
      HC.toCode schemaName <> " ="

  pure $
    HC.lines
      ( declType
          : declImpl
          : map (HC.indent 2) bodyLines
      )

moduleNameForType :: HC.TypeName -> CodeGen HC.ModuleName
moduleNameForType typeName = do
  options <- asks codeGenOptions

  let
    name =
      moduleBaseName options <> "." <> HC.renderText typeName

  pure (HC.ModuleName name)

textType :: HC.TypeName
textType =
  HC.toTypeName "Text"
