module Main (main) where

import Agda.Compiler.Backend qualified as Agda
import Agda.Interaction.FindFile qualified as Agda
import Agda.Interaction.Imports qualified as Agda
import Agda.Interaction.JSON qualified as Json
import Agda.Interaction.Library qualified as Agda
import Agda.Interaction.Options qualified as Agda
import Agda.Interaction.Options.Warnings qualified as Agda
import Agda.Syntax.Abstract.Name qualified as Abstract
import Agda.Syntax.Common.Pretty qualified as Pretty
import Agda.Syntax.Concrete qualified as Concrete
import Agda.Syntax.Translation.AbstractToConcrete qualified as Agda
import Agda.Syntax.Translation.InternalToAbstract qualified as Agda
import Agda.Utils.FileName qualified as Agda
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as ByteString
import Data.Foldable
import Data.List (sort)
import Data.Monoid
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt
import System.Directory
import System.FilePath
import System.FilePath.Find qualified as Find
import System.IO

--------------------------------------------------------------------------------
-- Extract summary information about definitions from Agda library

loadLibraryFileContent :: FilePath -> Agda.TCM ([FilePath], Agda.OptionsPragma)
loadLibraryFileContent currentDirectory = do
  [libraryFile] <- Agda.libToTCM $ Agda.mkLibM [] $ Agda.getAgdaLibFiles' currentDirectory
  pure (libraryFile._libIncludes, libraryFile._libPragmas)

findAllModulePath :: [FilePath] -> IO [FilePath]
findAllModulePath includePaths = do
  sort . map Find.infoPath . concat <$> forM includePaths \includePath -> do
    Find.fold
      (pure True)
      ( \foundAgdaFiles foundFile ->
          if Find.evalClause (isExtensionOf ".agda" <$> Find.filePath) foundFile
            then foundFile : foundAgdaFiles
            else foundAgdaFiles
      )
      []
      includePath

typeCheckModule :: FilePath -> Agda.TCM Agda.Interface
typeCheckModule moduleFilePath = do
  let agdaOptions =
        Agda.defaultOptions
          { Agda.optTraceImports = 0,
            Agda.optPragmaOptions =
              Agda.defaultPragmaOptions
                { Agda._optWarningMode = Agda.defaultWarningMode {Agda._warningSet = Agda.noWarnings}
                }
          }

  moduleAbsolutePath <- liftIO $ Agda.absolute moduleFilePath
  Agda.setCommandLineOptions agdaOptions
  source <- Agda.parseSource (Agda.SourceFile moduleAbsolutePath)
  typeCheckResult <- Agda.typeCheckMain Agda.TypeCheck source
  pure $ Agda.crInterface typeCheckResult

data DefinitionKind
  = Axiom
  | Function
  | Datatype
  | Constructor
  | Record
  | Primitive

data DefinitionSummary = DefinitionSummary
  { name :: Abstract.Name,
    topLevelModuleName :: Concrete.QName,
    moduleName :: Concrete.QName,
    qualifiedName :: Concrete.QName,
    kind :: DefinitionKind,
    signature :: Concrete.Expr
  }

definitionKind :: Agda.Defn -> Maybe DefinitionKind
definitionKind = \case
  Agda.DatatypeDefn {} -> Just Datatype
  Agda.RecordDefn {} -> Just Record
  Agda.ConstructorDefn {} -> Just Constructor
  Agda.AxiomDefn {} -> Just Axiom
  Agda.PrimitiveDefn {} -> Just Primitive
  Agda.PrimitiveSortDefn {} -> Just Primitive
  Agda.FunctionDefn defn
    -- Generated functions are not interesting for us
    | Just _ <- defn._funIsKanOp -> Nothing
    | Just _ <- defn._funWith -> Nothing
    | Just _ <- defn._funExtLam -> Nothing
    | otherwise -> Just Function
  _ -> Nothing

summarizeInterface :: Agda.Interface -> Agda.TCM [DefinitionSummary]
summarizeInterface interface = do
  let topLevelModuleName = Agda.mnameToConcrete interface.iModuleName
      definitions = interface.iSignature._sigDefinitions

  let Ap action = flip foldMap definitions \definition -> Ap do
        case definitionKind definition.theDef of
          Nothing -> pure mempty
          Just kind -> do
            signature <- Agda.reify definition.defType
            signature' <- Agda.abstractToConcrete_ signature
            let qname = definition.defName
                summary =
                  DefinitionSummary
                    { name = qname.qnameName,
                      topLevelModuleName,
                      moduleName = Agda.mnameToConcrete qname.qnameModule,
                      kind,
                      qualifiedName = Agda.qnameToConcrete qname,
                      signature = signature'
                    }
            pure $ Endo (summary :)

  flip appEndo [] <$> action

--------------------------------------------------------------------------------
-- Output the summaries as JSONL documents

data Document = Document
  { pageContent :: String,
    metadata :: Metadata
  }
  deriving (Generic)

data Metadata = Metadata
  { name :: String,
    topLevelModuleName :: String,
    moduleName :: String,
    qualifiedName :: String,
    kind :: String,
    signature :: String
  }
  deriving (Generic)

instance Json.ToJSON Metadata where
  toEncoding = Json.genericToEncoding Json.defaultOptions {Json.fieldLabelModifier = Json.camelTo2 '_'}

instance Json.ToJSON Document where
  toEncoding = Json.genericToEncoding Json.defaultOptions {Json.fieldLabelModifier = Json.camelTo2 '_'}

formatDefinitionSummary :: DefinitionSummary -> Document
formatDefinitionSummary summary = Document {..}
  where
    renderOneLine = Pretty.renderStyle Pretty.style {Pretty.mode = Pretty.OneLineMode}
    metadata = Metadata {..}
    pageContent = renderOneLine $ Pretty.pretty summary.name <> " : " <> Pretty.pretty summary.signature
    name = renderOneLine $ Pretty.pretty summary.name
    topLevelModuleName = renderOneLine $ Pretty.pretty summary.topLevelModuleName
    moduleName = renderOneLine $ Pretty.pretty summary.moduleName
    qualifiedName = renderOneLine $ Pretty.pretty summary.qualifiedName
    kind = case summary.kind of
      Axiom -> "axiom"
      Function -> "function"
      Datatype -> "datatype"
      Constructor -> "constructor"
      Record -> "record"
      Primitive -> "primitive"
    signature = renderOneLine $ Pretty.pretty summary.signature

--------------------------------------------------------------------------------
-- Command line options

newtype ExtractorOptions = ExtractorOptions
  { outDirectory :: FilePath
  }

optionParser :: Opt.Parser ExtractorOptions
optionParser =
  ExtractorOptions
    <$> Opt.strOption (Opt.long "outDir" <> Opt.metavar "OUT_DIR" <> Opt.help "The directory to write the signatures to")

optionParserInfo :: Opt.ParserInfo ExtractorOptions
optionParserInfo =
  Opt.info (optionParser Opt.<**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Extract signatures from Agda library")

--------------------------------------------------------------------------------

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  extractorOptions <- Opt.execParser optionParserInfo

  let outDirectory = currentDirectory </> extractorOptions.outDirectory

  Agda.runTCMTop' do
    (includePaths, libOpts) <- loadLibraryFileContent currentDirectory

    Agda.checkAndSetOptionsFromPragma libOpts

    moduleFilePaths <- liftIO $ findAllModulePath includePaths

    for_ moduleFilePaths \moduleFilePath -> flip catchError (const $ pure ()) do
      interface <- typeCheckModule moduleFilePath
      summaries <- summarizeInterface interface
      let moduleName = interface.iModuleName
          summariesFilePath =
            outDirectory
              </> foldr ((</>) . show . Pretty.pretty) "" moduleName.mnameToList
                <.> "jsonl"
      liftIO do
        createDirectoryIfMissing True (takeDirectory summariesFilePath)
        withFile summariesFilePath WriteMode \handle -> do
          for_ summaries \summary -> do
            ByteString.hPutStr handle $ Json.encode $ formatDefinitionSummary summary
            ByteString.hPutStr handle "\n"
