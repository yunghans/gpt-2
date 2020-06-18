{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language ViewPatterns #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM
import           Control.Exception (SomeException)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch (catch, MonadMask(..), ExitCase(..))
import           Control.Monad.IO.Class
import qualified Control.Monad.State.Class as S
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.IO.Handle
import           Network.IRC.CTCP
import           Network.IRC.Client
import           System.Exit
import           System.IO
import           System.IO.Unsafe

import           GPT2
import           LogFormat

kChannel :: Text
kChannel = "#lw-gpt"

type Channel = Text

data MyState = MyState {
  _stContext :: Text,
  _stLastWakeup :: Maybe UTCTime,
  _stLastChat :: UTCTime,
  _stPeriod :: Int
  }

stContext f (MyState{..}) = (\_stContext -> MyState {..}) <$> f _stContext
stLastWakeup f (MyState{..}) = (\_stLastWakeup -> MyState {..}) <$> f _stLastWakeup
stLastChat f (MyState{..}) = (\_stLastChat -> MyState {..}) <$> f _stLastChat
stPeriod f (MyState{..}) = (\_stPeriod -> MyState {..}) <$> f _stPeriod

portalSeconds :: NominalDiffTime
portalSeconds = 60 * 60 -- 1 hour

delaySeconds :: MonadIO m => Int -> m ()
delaySeconds n = liftIO (threadDelay (n * 1000000))

shutupThread :: IRC MyState ()
shutupThread = go False
  where
    go awake = do
      liftIO (threadDelay (1 * 1000000))
      now <- liftIO getCurrentTime
      lastWakeup <- use stLastWakeup
      case lastWakeup of
        Just wk
          | diffUTCTime now wk <= portalSeconds -> go True
        _ -> when awake (send (Privmsg kChannel (Right ("Shutting up...")))) >> go False

withMVarIRC :: (MonadIO m, MonadMask m) => MVar a -> (a -> m a) -> m a
withMVarIRC v k = do
  let start = liftIO (takeMVar v)
      end x (ExitCaseSuccess y) = liftIO (putMVar v y)
      end x _ = liftIO (putMVar v x)
      middle x = k x
  fst <$> generalBracket start end middle

feepbot :: Text -> IRC MyState ()
feepbot nick = void . fork $ withMVarIRC lock $ \() -> do
  delaySeconds 1
  now <- liftIO getCurrentTime
  ctx <- use stContext
  let
    prompt = Msg (formatTimestamp now) nick ""
    go = do
      (newctx, msg) <- sampleMessageWithPrompt ctx prompt
      if ("Wikipedia" `T.isSuffixOf` mtext msg) then
        putStrLn ("redrawing " ++ show msg) >> go
        else
        return (newctx, msg)
  (newctx, msg) <- liftIO go
  stContext .= (newctx <> formatMsg msg <> "\n")
  sendMsg kChannel msg
  where
    lock :: MVar ()
    lock = unsafePerformIO (newMVar ())

extendContext :: Msg -> IRC MyState ()
extendContext msg = do
  let line = formatMsg msg <> "\n"
  stContext %= (<> line)
  when (T.isInfixOf "http" (mtext msg)
        || T.isPrefixOf ".g" (mtext msg)
        || T.isPrefixOf ".wp" (mtext msg)) $ do
    feepbot "feepbot"
  when (T.isPrefixOf "!" (mtext msg)) $ do
    feepbot "Oborbot"
  newctx <- use stContext
  liftIO (T.writeFile "context.txt" newctx)

getCanSpeak :: IRC MyState Bool
getCanSpeak = do
  now <- liftIO getCurrentTime
  lastWakeup <- use stLastWakeup
  lastChat <- use stLastChat
  ctx <- use stContext
  period <- use stPeriod
  case lastWakeup of
    Just wk -> return ((diffUTCTime now wk <= portalSeconds)
                       && (diffUTCTime now lastChat >= fromIntegral period)
                       && T.isSuffixOf "\n" ctx)
    Nothing -> return False

sampleThread :: IRC MyState ()
sampleThread = forever (go
                        `catch` (\(e :: GPT2.Timeout) -> sendLine kChannel "Timed out")
                        `catch` (\(e :: SomeException) -> liftIO (print e)))
  where
    normal = do
      guard =<< getCanSpeak
      ctx <- use stContext
      (newctx, msg) <- liftIO (sampleMessage ctx)
      ctx' <- use stContext
      guard (ctx == ctx') -- try again if someone said something in the mean time
      stContext .= newctx

      now <- liftIO getCurrentTime
      let msg' = msg{mtime = formatTimestamp now}
      sendMsg kChannel msg'
      extendContext msg'
      stLastChat .= now

    go = do
      liftIO (threadDelay (1 * 1000000))
      normal <|> pure()

noping :: Text -> Text
noping us
  | T.null us = us
  | otherwise = T.take 1 us <> "\x2060" <> T.drop 1 us

cleanzwsp :: Text -> Text
cleanzwsp = T.filter (/= '\x2060')

sendMsg :: Channel -> Msg -> IRC MyState ()
sendMsg chan Msg{..}
  | muser `elem` ["*", ">>"] = sendLine chan (muser <> " " <> noping mtext)
  | otherwise = sendLine chan ("<" <> noping muser <> "> " <> mtext)

sendLine :: Channel -> Text -> IRC MyState ()
sendLine chan msg
  | totalLength > lengthLimit = do
      send (Privmsg chan (Right $ T.dropEnd overage msg <> "…"))
      sendLine chan ("…" <> T.takeEnd overage msg)
  | otherwise = send (Privmsg chan (Right msg))
  where
    lengthLimit = 510 - 58
    totalLength = T.length ("PRIVMSG " <> chan <> " :" <> msg)
    overage = totalLength - lengthLimit + 3

gwernpaste :: Channel -> Text -> IRC MyState ()
gwernpaste chan prompt = void $ fork (go
                                      `catch` (\(e :: GPT2.Timeout) -> sendLine kChannel "Timed out")
                                      `catch` (\(e :: SomeException) -> liftIO (print e)))
  where
    go = do
      orig_ctx <- pure "" -- use stContext
      ls <- liftIO (sampleGwernpaste orig_ctx prompt)
      for_ ls $ \line -> do
        sendMsg chan line
        extendContext line

gwernpaste' :: Channel -> Text -> IRC MyState ()
gwernpaste' chan prompt = void $ fork (go
                                      `catch` (\(e :: GPT2.Timeout) -> sendLine kChannel "Timed out")
                                      `catch` (\(e :: SomeException) -> liftIO (print e)))
  where
    go = do
      orig_ctx <- use stContext
      ls <- liftIO (sampleGwernpaste orig_ctx prompt)
      for_ ls $ \line -> do
        sendMsg chan line
        extendContext line

completion :: Channel -> Text -> Text -> IRC MyState ()
completion chan user prompt = void $ fork $ do
  ctx <- use stContext
  now <- liftIO getCurrentTime
  let
    (part1, T.drop 1 -> part2) = T.break isSpace prompt
    lctx
        | T.isPrefixOf "<" part1 && T.isSuffixOf ">" part1 =
            let user' = T.drop 1 $ T.dropEnd 1 $ part1
                prompt' = part2
            in Msg (formatTimestamp now) user' prompt'
        | part1 `elem` ["*", ">>"] = Msg (formatTimestamp now) part1 part2
        | otherwise = Msg (formatTimestamp now) user prompt
  (newctx, msg) <- liftIO (sampleMessageWithPrompt ctx lctx)
  sendMsg chan msg
  extendContext msg

getChannel :: Source Text -> Channel
getChannel (Channel ch u) = ch
getChannel (User u) = u

getUser :: Source Text -> Text
getUser (Channel ch u) = u
getUser (User u) = u

tshow x = T.pack (show x)

pingThread :: TVar (Maybe Text) -> IRC MyState ()
pingThread pingServer = go
  where
    go = do
      liftIO (threadDelay (60 * 1000000))
      pingOnce `catch` (\(e :: SomeException) -> liftIO (print e))
      go

    pingOnce = do
      server <- liftIO (atomically $ do
                           may_server <- readTVar pingServer
                           case may_server of
                             Just s -> return s
                             Nothing -> retry)
      send (Ping server Nothing)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  [nick, pass] <- T.splitOn "/" <$> T.readFile "identity.txt"

  pingServer <- newTVarIO Nothing

  let conn = tlsConnection (WithDefaultConfig "chat.freenode.net" 7000)
             & username .~ nick
             & realname .~ nick
             & password .~ Just pass
             & logfunc .~ stdoutLogger
             & timeout .~ 24 * 3600
  let cfg  = defaultInstanceConfig "gpt2"
             & channels .~ []
             & handlers %~ (myhandlers ++)

      myhandlers = [
        EventHandler (matchType _Notice) handleNotice,
        EventHandler (matchType _Privmsg) handleMessage,
        EventHandler (matchType _Ping) handlePing
        ]

      handleNotice (User "NickServ") (_, Right msg)
        | T.isPrefixOf "You are now identified" msg = do
            send (Join kChannel)
            liftIO (print msg)
            fork shutupThread
            fork sampleThread
            fork (pingThread pingServer)
            return ()
      handleNotice _ _ = pure()

      handlePing source (sname, ex) = liftIO $ do
        print (source, sname, ex)
        atomically (writeTVar pingServer (Just sname))

      handleMessage :: Source Text
                         -> (Text, Either CTCPByteString Text)
                         -> IRC MyState ()
      handleMessage _ (_, Right "gpt2: go away") =
        stLastWakeup .= Nothing
      handleMessage _ (_, Right "gpt2: shut up") =
        stLastWakeup .= Nothing
      handleMessage _ (_, Right "gpt2: come back") = do
        now <- liftIO getCurrentTime
        stLastWakeup .= Just now
      handleMessage _ (_, Right "gpt2: wake up") = do
        now <- liftIO getCurrentTime
        stLastWakeup .= Just now

      handleMessage src (_, Right msg)
        | T.isPrefixOf "@" msg = case cmd of
            "@clear" -> do
              stContext .= "\n"
              sendLine (getChannel src) "Forgotten."
            "@info" -> do
              (model, ckpt) <- liftIO getInfo
              sendLine (getChannel src) ("GPT-2: model=" <> model <> " checkpoint=" <> ckpt)
            "@period" ->
              case args of
                [] -> do
                  period <- use stPeriod
                  sendLine (getChannel src) ("Speaking every " <> tshow period <> " seconds")
                [ptxt] | T.all isDigit ptxt -> do
                           let period = max 5 (read (T.unpack ptxt)) :: Int
                           stPeriod .= period
                           sendLine (getChannel src) ("Speaking every " <> tshow period <> " seconds")
                _ -> sendLine (getChannel src) ("Expected an integer (minimum 5)")
            "@gwernpaste" ->
              case args of
                ("+ctx" : pwords) ->
                  gwernpaste' (getChannel src) (T.unwords pwords)
                pwords ->
                  gwernpaste (getChannel src) (T.unwords pwords)
            "@complete" ->
              completion (getChannel src) (getUser src) (cleanzwsp $ T.unwords args)
            _ -> pure()
        where
          (cmd:args) = T.words msg
      -- handleMessage _ (_, Right "@reload") = do
      --   disconnect
      --   liftIO exitSuccess
      handleMessage (Channel ch u) (_, Right msg)
        | u == "Robomot" = pure()
        | ch == kChannel
        = do now <- liftIO getCurrentTime
             let timestamp = formatTimestamp now
             extendContext (Msg timestamp u (cleanzwsp msg))
      handleMessage s m = liftIO (print (s,m))

  now <- getCurrentTime
  oldctx <- T.readFile "context.txt" <|> return ""
  runClient conn cfg (MyState oldctx Nothing now 15)
