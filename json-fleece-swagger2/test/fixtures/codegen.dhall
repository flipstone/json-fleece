let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "Guard"
      , inputFileName = "${rootDir}/definition-ref-alias.json"
      , destination = rootDir
      }
