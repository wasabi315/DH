{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Agda.Compiler.Backend
import Agda.Interaction.FindFile
import Agda.Interaction.Imports
import Agda.Interaction.Library
import Agda.Interaction.Options
import Agda.Interaction.Options.Warnings (defaultWarningMode, noWarnings)
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Position
import Agda.Syntax.Translation.InternalToAbstract
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.FileName
import Agda.Utils.Impossible
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Options.Applicative
import System.Directory
import System.FilePath
import System.FilePath.Find qualified as Find
import System.IO

newtype ExtractorOptions = ExtractorOptions
  { outDirectory :: FilePath
  }

optionParser :: Parser ExtractorOptions
optionParser =
  ExtractorOptions
    <$> strOption (long "outDir" <> metavar "OUT_DIR" <> help "The directory to write the signatures to")

optionParserInfo :: ParserInfo ExtractorOptions
optionParserInfo =
  info (optionParser <**> helper) (fullDesc <> progDesc "Extract signatures from Agda library")

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  extractorOptions <- execParser optionParserInfo

  let outDirectory = currentDirectory </> extractorOptions.outDirectory

  runTCMTop' do
    -- Read the library file.
    agdaLibFiles <- libToTCM $ mkLibM [] $ getAgdaLibFiles' currentDirectory
    AgdaLibFile
      { _libIncludes = includePaths,
        _libPragmas = libOpts
      } <- case agdaLibFiles of
      [l] -> pure l
      [] -> throwError $ Exception noRange "No library found to build"
      _ -> __IMPOSSIBLE__

    checkAndSetOptionsFromPragma libOpts

    -- Find all modules in the include paths of the library.
    moduleFilePaths <-
      sort . map Find.infoPath . concat <$> forM includePaths \includePath -> liftIO do
        Find.fold
          (pure True)
          ( \foundAgdaFiles foundFile ->
              if Find.evalClause (isExtensionOf ".agda" <$> Find.filePath) foundFile
                then foundFile : foundAgdaFiles
                else foundAgdaFiles
          )
          []
          includePath

    let agdaOptions =
          defaultOptions
            { optOnlyScopeChecking = True,
              optTraceImports = 0,
              optPragmaOptions =
                defaultPragmaOptions
                  { _optWarningMode = defaultWarningMode {_warningSet = noWarnings}
                  }
            }

    for_ moduleFilePaths \moduleFilePath -> flip catchError (const $ pure ()) do
      moduleAbsolutePath <- liftIO $ absolute moduleFilePath
      setCommandLineOptions agdaOptions
      source <- parseSource (SourceFile moduleAbsolutePath)
      typeCheckResult <- typeCheckMain TypeCheck source
      let interface = crInterface typeCheckResult
          currentModuleName = iModuleName interface
          definitions = _sigDefinitions $ iSignature interface
      signatures <- forM definitions \definition -> do
        type_ <- reify $ defType definition
        prettyTCM type_

      let signatureFileDirectory =
            outDirectory </> foldr ((</>) . show . pretty) "" (init $ mnameToList currentModuleName)
          signatureFileName = show (pretty $ last $ mnameToList currentModuleName) <.> "md"
      liftIO do
        createDirectoryIfMissing True signatureFileDirectory
        withFile (signatureFileDirectory </> signatureFileName) WriteMode \handle -> do
          hPrint handle $ "# Signatures found in " <> pretty currentModuleName <> "\n"
          void $ flip HashMap.traverseWithKey signatures \name signature -> do
            hPrint handle $ "## `" <> pretty (qnameToConcrete name) <> "`\n"
            hPutStrLn handle "```agda"
            hPrint handle $ pretty (qnameToConcrete name) <+> ":" <?> signature
            hPutStrLn handle "```\n"
