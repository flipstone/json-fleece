let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "StarTrek"
      , inputFileName = "${rootDir}/star-trek.yaml"
      , destination = rootDir
      }
