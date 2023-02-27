module Fleece.CodeGenUtil.Executable
  ( codeGenMain
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Yaml.Aeson as YA
import qualified Options.Applicative as Opt
import System.Console.Isocline (readline)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath (takeDirectory, takeExtension, (</>))

import qualified Fleece.CodeGenUtil as CGU

data Options = Options
  { moduleBaseName :: T.Text
  , fileName :: FilePath
  , mode :: Mode
  , destination :: FilePath
  , noConfirm :: Bool
  }

data Mode
  = Preview
  | Run

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    ( Options
        <$> Opt.strArgument (Opt.metavar "MODULE_BASE_NAME")
        <*> Opt.strArgument (Opt.metavar "OPEN_API_3.yml")
        <*> modeParser
        <*> destinationParser
        <*> Opt.flag False True (Opt.long "yes")
    )
    mempty

modeParser :: Opt.Parser Mode
modeParser =
  Opt.flag
    Run
    Preview
    (Opt.long "preview" <> Opt.short 'p')

destinationParser :: Opt.Parser FilePath
destinationParser =
  Opt.strOption
    ( Opt.long "dest"
        <> Opt.short 'd'
        <> Opt.metavar "DESTINATION"
        <> Opt.value "."
    )

codeGenMain :: Aeson.FromJSON a => (a -> CGU.CodeGen CGU.Modules) -> IO ()
codeGenMain generateModules = do
  options <- Opt.customExecParser parserPrefs optionsInfo
  source <- loadSourceOrDie (fileName options)

  let
    codeGenOptions =
      CGU.CodeGenOptions
        { CGU.moduleBaseName = moduleBaseName options
        }

  case CGU.runCodeGen codeGenOptions (generateModules source) of
    Left err -> Exit.die (show err)
    Right modules ->
      case mode options of
        Preview -> traverse_ (uncurry previewModule) modules
        Run -> createFiles options modules

previewModule :: FilePath -> CGU.HaskellCode -> IO ()
previewModule path code = do
  putStrLn "===="
  putStrLn path
  putStrLn "===="
  LTIO.putStrLn (CGU.renderLazyText code)

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

loadSourceOrDie :: Aeson.FromJSON a => FilePath -> IO a
loadSourceOrDie path = do
  let
    showYamlError =
      fmap (either (Left . show) Right)

  errOrSource <-
    case map Char.toLower (takeExtension path) of
      ".yaml" -> showYamlError $ YA.decodeFileEither path
      ".yml" -> showYamlError $ YA.decodeFileEither path
      ".json" -> Aeson.eitherDecodeFileStrict path
      ext -> pure . Left $ "Unsupported file format: " <> show ext

  case errOrSource of
    Left err -> Exit.die (show err)
    Right source -> pure source

createFiles :: Options -> [(FilePath, CGU.HaskellCode)] -> IO ()
createFiles options modules = do
  confirm options modules
  traverse_ (uncurry $ createFile options) modules

createFile :: Options -> FilePath -> CGU.HaskellCode -> IO ()
createFile options path code = do
  let
    fullPath =
      destination options </> path

    directory =
      takeDirectory fullPath

  Dir.createDirectoryIfMissing True directory
  LTIO.writeFile fullPath (CGU.renderLazyText code)

confirm :: Options -> [(FilePath, CGU.HaskellCode)] -> IO ()
confirm options modules =
  if noConfirm options
    then pure ()
    else do
      let
        go = do
          answer <- readline "Is this what you want? (yes/no)"

          case map Char.toLower answer of
            "yes" -> pure ()
            "no" -> Exit.die "Aborting at user request"
            _ -> go

      putStrLn $
        "This will create "
          <> show (length modules)
          <> " total files in the destination directory."

      go
