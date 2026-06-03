let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "SelectedItemsExample"
      , inputFileName = "${rootDir}/selected-items-example.yaml"
      , destination = rootDir
      , selectedItems = CodeGen.selectedItems
          [ CodeGen.operationId "listPets"
          , CodeGen.operationId "createPet"
          , CodeGen.operationId "getInventory"
          -- getPet's `petId` path param is typed by the PetId component; PetId is
          -- reachable only through that param, so this exercises the filter
          -- following operation-parameter schema references.
          , CodeGen.operationId "getPet"
          , CodeGen.pathMethod "/health" "GET"
          , CodeGen.component "Habitat"
          ]
      }
