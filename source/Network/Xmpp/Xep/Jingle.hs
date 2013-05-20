{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.Xmpp.Xep.Jingle where

import           Network.Xmpp.Xep.Jingle.Types

import           Control.Arrow (first)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.XML.Types
import           Data.XML.Pickle (ppUnpickleError)
import qualified Network.Xmpp as Xmpp
import           Network.Xmpp.Xep.Jingle.Picklers
import           Network.Xmpp.Xep.Jingle.Types
import           System.Log.Logger
import           System.Log.Logger (errorM, infoM, debugM)


equivClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivClasses p = unfoldr (\xs -> case xs of [] -> Nothing
                                            (y:ys) -> Just . first (y:)
                                                            $ partition (p y) ys)

serviceUnavailable :: Xmpp.StanzaError
serviceUnavailable = Xmpp.StanzaError Xmpp.Cancel Xmpp.ServiceUnavailable Nothing
                                      Nothing

badRequest :: Xmpp.StanzaError
badRequest = Xmpp.StanzaError Xmpp.Cancel Xmpp.BadRequest Nothing
                              Nothing
dummyContentHandler :: ApplicationHandler
dummyContentHandler = ApplicationHandler { chTransportType = Datagram
                                         , chNamespace = "dummycontent"
                                         , chHandler = \_ -> return ()
                                         }


errorUnavailable :: Xmpp.IQRequestTicket -> Xmpp.Session -> IO ()
errorUnavailable ticket session = void $ Xmpp.answerIQ ticket
                                     (Left serviceUnavailable)

errorBadRequest :: Xmpp.IQRequestTicket -> IO ()
errorBadRequest ticket = void $ Xmpp.answerIQ ticket (Left badRequest)

jingleTerminate reasonType sid =
    Jingle { action = SessionTerminate
           , initiator = Nothing
           , responder = Nothing
           , sid = sid
           , content = []
           , reason = Just JingleReason { reasonType = reasonType
                                        , reasonText = Nothing
                                        , reasonElement = Nothing
                                        }
           , jinglePayload = []
           }

startJingle :: HandlerFunc
            -> (Xmpp.IQRequest -> IO Bool)
            -> Xmpp.Session
            -> IO (Maybe JingleHandler)
startJingle handleContent policy xmppSession = do
    sessions <- newTVarIO Map.empty
    infoM "Pontarius.Xmpp.Jingle" "getting IQ channel"
    chan' <- Xmpp.listenIQChan Xmpp.Set "urn:xmpp:jingle:1" xmppSession
    case chan' of
        Left _ -> errorM "Pontarius.Xmpp.Jingle"
                         "Jingle channel is already in use"
                  >> return Nothing
        Right chan -> do
            infoM "Pontarius.Xmpp.Jingle" "Got channel, starting worker thread"
            thread <- forkIO . forever $ do
                ticket <- atomically $ readTChan chan
                tid <- myThreadId
                let request = Xmpp.iqRequestBody ticket
                    jh = JingleHandler
                               { jingleSessions = sessions
                               , jingleThread = tid
                               , jingleXmppSession = xmppSession
                               }
                case unpickleElem xpJingle $ Xmpp.iqRequestPayload request of
                    Left e -> do
                        errorM "Pontarius.Xmpp.Jingle" $
                                "Could not unpickle jingle element:\n"
                                  ++ ppUnpickleError e
                    Right ji -> case action ji of
                        SessionInitiate -> sessionInitiate sessions ji ticket jh
                        _               -> handleSession sessions ji ticket jh
            return . Just $ JingleHandler
                               { jingleSessions = sessions
                               , jingleThread = thread
                               , jingleXmppSession = xmppSession
                               }

  where
    sessionInitiate sessions ji ticket jh = do
        debugM "Pontarius.Xmpp.Jingle" $ "Recevied session-initiate: " ++
               Text.unpack (sid ji)
        sess <- atomically $ readTVar sessions
        case Map.lookup (sid ji) sess of
            Nothing -> do
                stateRef <- newTVarIO PENDING
                requestsRef <- newTChanIO
                mbs <- newSession jh ji ticket stateRef requestsRef
                case mbs of
                    Nothing -> return ()
                    Just s -> do
                        atomically . modifyTVar sessions $ Map.insert (sSid s) s
            Just _ -> return () -- TODO
    handleSession sessions ji ticket jh = do
        sess <- atomically $ readTVar sessions
        case Map.lookup (sid ji) sess of
            Nothing -> do
                errorM "Pontarius.Xmpp.Jingle" $ "Session not found: " ++
                       show ( sid ji)
                checkPolicy ticket $ do return () -- TODO?
            Just session -> do
                debugM "Pontarius.Xmpp.Jingle"
                          $ "Got message for session " ++ Text.unpack (sid ji)
                if (Xmpp.iqRequestFrom $ Xmpp.iqRequestBody ticket)
                              == (Just $ sRemote session)
                    then if isSessionPing ji then do
                           Xmpp.answerIQ ticket (Right Nothing)
                           return ()
                         else sRequests session session ticket ji
                    else errorUnavailable ticket xmppSession
    checkPolicy ticket f = do
        answer <- policy (Xmpp.iqRequestBody ticket)
        if answer then f else errorUnavailable ticket xmppSession
    newSession jh ji ticket stateRef requestsRef =
        case ( Xmpp.iqRequestFrom $ Xmpp.iqRequestBody ticket) of
            (Just remote) -> do
                Xmpp.answerIQ ticket (Right Nothing)
                handle <- handleContent remote ji stateRef requestsRef jh
                case handle of
                    Just handlerFunc -> do
                        return . Just $ Session { sState = stateRef
                                , sSid = sid ji
                                , sRemote = remote
                                , sRequests = handlerFunc
                                }
                    Nothing -> return Nothing -- The handler has to decline the
                                              -- session
            _ -> errorBadRequest ticket >> return Nothing
    isSessionPing Jingle{ action = SessionInfo
                        , initiator = Nothing
                        , responder = Nothing
                        , content = []
                        , reason = Nothing
                        } =  True
    isSessionPing _ = False

addSession :: Session -> JingleHandler -> IO Bool
addSession s JingleHandler{jingleSessions = href} = atomically $ do
    hs <- readTVar href
    case Map.member (sSid s) hs of
        True -> return False
        False -> do
            writeTVar href $ Map.insert (sSid s) s hs
            return True

-- | Remove session without sending session-terminate (e.g. when we received session-terminate)
endSession s jh = atomically $ modifyTVar (jingleSessions jh) (Map.delete s)

-- | Send session-terminate and end session
terminateSession sid JingleHandler{ jingleSessions = href
                                  , jingleXmppSession = xmppSession
                                  }
           reason = do
    s <- atomically $ do
        sessions <- readTVar href
        let (s, sessions') = Map.updateLookupWithKey (\_ _ -> Nothing) sid sessions
        writeTVar href sessions'
        return s
    case s of
        Nothing -> return ()
        Just s' -> do
            let terminate = Jingle { action = SessionTerminate
                                   , initiator = Nothing
                                   , responder = Nothing
                                   , sid = sSid s'
                                   , content = []
                                   , jinglePayload = []
                                   , reason = Just reason
                                   }
                terminateE = pickleElem xpJingle terminate
            _ <- Xmpp.sendIQ Nothing (Just $ sRemote s')
                        Xmpp.Set Nothing terminateE xmppSession
            return ()
