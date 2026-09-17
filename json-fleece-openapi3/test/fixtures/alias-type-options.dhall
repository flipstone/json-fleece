let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "Guard"
      , inputFileName = "${rootDir}/alias-type-options.yaml"
      , destination = rootDir
      , typeOptions =
          [ { type = "Guard.Types.AliasOfBase.AliasOfBase"
            , options = CodeGen.TypeOptions::{ deriveClasses = CodeGen.derive [ CodeGen.show ] }
            }
          ]
      }
