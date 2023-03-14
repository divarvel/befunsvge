{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Conduit (ConduitT, concatMapC, concatMapMC, mapC, mapMC, runConduit, sinkHandle, sourceHandle, transPipe, (.|))
import Config (Config (..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Relude
import State (DebugStep (..), DebuggerState (..), Position, renderDebuggerState, runStepDbg, setBreakpoints, startDebugger)

data DapRequest
  = Init (Req InitArguments)
  | Launch (Req LaunchArguments)
  | SetBreakpoints (Req SetBreakpointsArguments)
  deriving stock (Show, Generic)

instance FromJSON DapRequest where
  parseJSON = withObject "request" $ \o -> do
    command <- o .: "command" :: Parser Text
    case command of
      "initialize" -> Init <$> parseJSON (Object o)
      "launch" -> Launch <$> parseJSON (Object o)
      "setBreakpoints" -> SetBreakpoints <$> parseJSON (Object o)
      _ -> fail "unsupported command"

data Req a = Req
  { rseq :: Natural,
    command :: Text,
    arguments :: a
  }
  deriving stock (Show, Generic)

instance FromJSON a => FromJSON (Req a) where
  parseJSON = withObject "request" $ \o -> do
    rseq <- o .: "seq"
    command <- o .: "command"
    arguments <- o .: "arguments"
    pure Req {..}

data InitArguments = InitArguments
  { adapterID :: Text,
    clientID :: Text,
    clientName :: Text,
    columnsStartAt1 :: Bool,
    linesStartAt1 :: Bool,
    locale :: Text, -- todo
    pathFormat :: Text, -- todo
    supportsInvalidatedEvent :: Bool,
    supportsMemoryReferences :: Bool,
    supportsProgressReporting :: Bool,
    supportsRunInTerminalRequest :: Bool,
    supportsVariablePaging :: Bool,
    supportsVariableType :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype SetBreakpointsResponseR = SetBreakpointsResponseR
  { breakpoints :: [Breakpoint]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Breakpoint = Breakpoint
  { id :: Maybe Natural,
    verified :: Bool,
    message :: Maybe Text,
    source :: Maybe Source,
    line :: Maybe Natural,
    column :: Maybe Natural,
    endLine :: Maybe Natural,
    endColumn :: Maybe Natural
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Event a = Event
  { event :: Text,
    body :: a
  }
  deriving stock (Show)

instance ToJSON a => ToJSON (Event a) where
  toJSON Event {..} =
    object
      [ "type" .= ("event" :: Text),
        "event" .= event,
        "body" .= body
      ]

data StoppedEventData = StoppedEventData
  { reason :: Text,
    description :: Maybe Text,
    hitBreakpointIds :: Maybe [Int]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

newtype TerminatedEventData = TerminatedEventData
  { restart :: Maybe ()
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data DapEvent
  = StoppedEvent StoppedEventData
  | TerminatedEvent TerminatedEventData
  | OutputEvent ()
  deriving stock (Show)

instance ToJSON DapEvent where
  toJSON = \case
    StoppedEvent e -> toJSON $ Event "stopped" e
    TerminatedEvent e -> toJSON $ Event "terminated" e
    OutputEvent e -> toJSON $ Event "output" e

data DapResponse
  = InitDone (Res Capabilities)
  | LaunchDone (Res ())
  | SetBreakpointsResponse (Res SetBreakpointsResponseR)
  | Event' DapEvent
  deriving stock (Show)

instance ToJSON DapResponse where
  toJSON = \case
    InitDone r -> toJSON r
    LaunchDone r -> toJSON r
    SetBreakpointsResponse r -> toJSON r
    Event' e -> toJSON e

data Res a = Res
  { requestSeq :: Natural,
    command :: Text,
    message :: Maybe Text,
    success :: Bool,
    body :: a
  }
  deriving stock (Show, Generic)

instance ToJSON a => ToJSON (Res a) where
  toJSON Res {..} =
    object
      [ "type" .= ("response" :: Text),
        "request_seq" .= requestSeq,
        "command" .= command,
        "message" .= message,
        "success" .= success,
        "body" .= body
      ]

data Capabilities = Capabilities
  { supportsConfigurationDoneRequest :: Maybe Bool,
    supportsFunctionBreakpoints :: Maybe Bool,
    supportsConditionalBreakpoints :: Maybe Bool,
    supportsHitConditionalBreakpoints :: Maybe Bool,
    supportsEvaluateForHovers :: Maybe Bool,
    exceptionBreakpointFilters :: Maybe [Void], -- Maybe [ExceptionBreakpointsFilter],
    supportsStepBack :: Maybe Bool,
    supportsSetVariable :: Maybe Bool,
    supportsRestartFrame :: Maybe Bool,
    supportsGotoTargetsRequest :: Maybe Bool,
    supportsStepInTargetsRequest :: Maybe Bool,
    supportsCompletionsRequest :: Maybe Bool,
    completionTriggerCharacters :: Maybe [Text],
    supportsModulesRequest :: Maybe Bool,
    additionalModuleColumns :: Maybe [ColumnDescriptor],
    supportedChecksumAlgorithms :: Maybe [ChecksumAlgorithm],
    supportsRestartRequest :: Maybe Bool,
    supportsExceptionOptions :: Maybe Bool,
    supportsValueFormattingOptions :: Maybe Bool,
    supportsExceptionInfoRequest :: Maybe Bool,
    supportTerminateDebuggee :: Maybe Bool,
    supportSuspendDebuggee :: Maybe Bool,
    supportsDelayedStackTraceLoading :: Maybe Bool,
    supportsLoadedSourcesRequest :: Maybe Bool,
    supportsLogPoints :: Maybe Bool,
    supportsTerminateThreadsRequest :: Maybe Bool,
    supportsSetExpression :: Maybe Bool,
    supportsTerminateRequest :: Maybe Bool,
    supportsDataBreakpoints :: Maybe Bool,
    supportsReadMemoryRequest :: Maybe Bool,
    supportsWriteMemoryRequest :: Maybe Bool,
    supportsDisassembleRequest :: Maybe Bool,
    supportsCancelRequest :: Maybe Bool,
    supportsBreakpointLocationsRequest :: Maybe Bool,
    supportsClipboardContext :: Maybe Bool,
    supportsSteppingGranularity :: Maybe Bool,
    supportsInstructionBreakpoints :: Maybe Bool,
    supportsExceptionFilterOptions :: Maybe Bool,
    supportsSingleThreadExecutionRequests :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

defaultCaps :: Capabilities
defaultCaps =
  Capabilities
    { supportsConfigurationDoneRequest = Just True,
      supportsFunctionBreakpoints = Just False,
      supportsConditionalBreakpoints = Just True,
      supportsHitConditionalBreakpoints = Just True,
      supportsEvaluateForHovers = Nothing,
      exceptionBreakpointFilters = Nothing,
      supportsStepBack = Just True,
      supportsSetVariable = Nothing,
      supportsRestartFrame = Nothing,
      supportsGotoTargetsRequest = Nothing,
      supportsStepInTargetsRequest = Nothing,
      supportsCompletionsRequest = Nothing,
      completionTriggerCharacters = Nothing,
      supportsModulesRequest = Nothing,
      additionalModuleColumns = Nothing,
      supportedChecksumAlgorithms = Nothing,
      supportsRestartRequest = Nothing,
      supportsExceptionOptions = Nothing,
      supportsValueFormattingOptions = Nothing,
      supportsExceptionInfoRequest = Nothing,
      supportTerminateDebuggee = Just True,
      supportSuspendDebuggee = Just True,
      supportsDelayedStackTraceLoading = Nothing,
      supportsLoadedSourcesRequest = Nothing,
      supportsLogPoints = Just True,
      supportsTerminateThreadsRequest = Nothing,
      supportsSetExpression = Nothing,
      supportsTerminateRequest = Nothing,
      supportsDataBreakpoints = Nothing,
      supportsReadMemoryRequest = Just True,
      supportsWriteMemoryRequest = Just True,
      supportsDisassembleRequest = Nothing,
      supportsCancelRequest = Nothing,
      supportsBreakpointLocationsRequest = Just True,
      supportsClipboardContext = Nothing,
      supportsSteppingGranularity = Just True,
      supportsInstructionBreakpoints = Just True,
      supportsExceptionFilterOptions = Nothing,
      supportsSingleThreadExecutionRequests = Nothing
    }

data ColumnDescriptorType = CDTString | CDTNumber | CDTBoolean | CDTUnixTimestampUTC
  deriving stock (Show)

instance ToJSON ColumnDescriptorType where
  toJSON = \case
    CDTString -> "string"
    CDTNumber -> "number"
    CDTBoolean -> "boolean"
    CDTUnixTimestampUTC -> "unixTimestampUTC"

data ColumnDescriptor = ColumnDescriptor
  { attributeName :: Text,
    label :: Text,
    format :: Maybe Text,
    ctype :: Maybe ColumnDescriptorType,
    width :: Maybe Natural
  }
  deriving stock (Show)

instance ToJSON ColumnDescriptor where
  toJSON ColumnDescriptor {..} =
    object
      [ "attributeName" .= attributeName,
        "label" .= label,
        "format" .= format,
        "type" .= ctype,
        "width" .= width
      ]

data ChecksumAlgorithm = MD5 | SHA1 | SHA256 | Timestamp
  deriving stock (Show)

instance ToJSON ChecksumAlgorithm where
  toJSON = \case
    MD5 -> "MD5"
    SHA1 -> "SHA1"
    SHA256 -> "SHA256"
    Timestamp -> "timestamp"

data LaunchArguments = LaunchArguments
  { noDebug :: Maybe Bool,
    program :: FilePath,
    height :: Int,
    width :: Int,
    maxIter :: Maybe Natural,
    seed :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data SetBreakpointsArguments = SetBreakpointsArguments
  { source :: Source,
    breakpoints :: Maybe [SourceBreakpoint],
    sourceModified :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype Source = Source
  { path :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SourceBreakpoint = SourceBreakpoint
  { line :: Natural,
    column :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

mkBreakpoints :: SetBreakpointsArguments -> (Set Position, SetBreakpointsResponseR)
mkBreakpoints SetBreakpointsArguments {..} =
  let mkBreakpoint SourceBreakpoint {..} =
        Breakpoint
          { id = Nothing,
            verified = False,
            message = Nothing,
            source = Just source,
            line = Just line,
            endLine = Just line,
            column = Just column,
            endColumn = Just column,
            ..
          }
   in ( fromList $ fold breakpoints <&> \SourceBreakpoint {..} -> (line, column),
        SetBreakpointsResponseR
          { breakpoints = foldMap (fmap mkBreakpoint) breakpoints,
            ..
          }
      )

type M = StateT (Maybe DebuggerState) IO

type C i o a = ConduitT i o M a

handleReq :: DapRequest -> M [DapResponse]
handleReq = \case
  Init r -> pure [handleInit r]
  Launch r -> handleLaunch r
  SetBreakpoints r -> one <$> handleSetBreakpoints r

handleInit :: Req InitArguments -> DapResponse
handleInit Req {..} =
  InitDone $
    Res
      { requestSeq = rseq,
        command = command,
        message = Nothing,
        success = True,
        body = defaultCaps
      }

handleLaunch :: Req LaunchArguments -> M [DapResponse]
handleLaunch Req {..} = do
  let LaunchArguments {..} = arguments
      source = Nothing
      config = Config {..}
  st <- liftIO $ startDebugger config program
  appendFileBS "log" "TRACE: started\n"
  put $ Just $ runStepDbg DebugContinue st
  xx <- get
  appendFileBS "log" $ "TRACE: paused" <> encodeUtf8 (maybe "NotStarted" renderDebuggerState xx) <> "\n"
  event <-
    get <&> \case
      Nothing -> Nothing
      Just st' -> case execState' st' of
        Left _ -> Just (TerminatedEvent $ TerminatedEventData Nothing)
        Right _ ->
          Just
            ( StoppedEvent $
                StoppedEventData
                  { reason = "breakpoint",
                    description = Nothing,
                    hitBreakpointIds = Nothing
                  }
            )
  pure $
    [ LaunchDone $
        Res
          { requestSeq = rseq,
            command = command,
            message = Nothing,
            success = True,
            body = ()
          }
    ]
      <> foldMap (one . Event') event

handleSetBreakpoints :: Req SetBreakpointsArguments -> M DapResponse
handleSetBreakpoints Req {..} = do
  let (bkps, body) = mkBreakpoints arguments
  modify (fmap $ setBreakpoints bkps)
  pure
    ( SetBreakpointsResponse $
        Res
          { requestSeq = rseq,
            command = command,
            message = Nothing,
            success = True,
            body = body
          }
    )

-- strip until `\r`, then strip `\r\n` twice
stripHeader :: ByteString -> ByteString
stripHeader = BS.drop 4 . BS.dropWhile (/= 13)

addHeader :: ByteString -> ByteString
addHeader content =
  "Content-Length: "
    <> show (BS.length content)
    <> "\r\n"
    <> "\r\n"
    <> content

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  appendFileBS "log" "===== START =====\n"
  runConduit . transPipe (evaluatingStateT Nothing) $
    sourceHandle stdin
      .| mapMC
        ( \bs -> do
            appendFileBS "log" ">>> input\n"
            appendFileBS "log" bs
            appendFileBS "log" "<<< input\n"
            pure bs
        )
      .| mapC (eitherDecode @DapRequest . fromStrict . stripHeader)
      .| concatMapC rightToMaybe
      .| concatMapMC handleReq
      .| mapC (addHeader . toStrict . encode)
      .| mapMC
        ( \bs -> do
            appendFileBS "log" "<<< output\n"
            appendFileBS "log" bs
            appendFileBS "log" ">>> output\n"
            pure bs
        )
      .| sinkHandle stdout