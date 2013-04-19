{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.Xmpp.Xep.Jingle where

import           Network.Xmpp.Xep.Jingle.Types

import           Control.Arrow (first)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           System.Log.Logger (errorM, infoM, debugM)

import qualified Network.Xmpp as Xmpp
import           Network.Xmpp.Xep.Jingle.Picklers
import           Network.Xmpp.Xep.Jingle.Types


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
                                     (Left serviceUnavailable) session

errorBadRequest :: Xmpp.IQRequestTicket -> Xmpp.Session -> IO ()
errorBadRequest ticket session = void $ Xmpp.answerIQ ticket
                                     (Left badRequest) session

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
           }

startJingle :: (Xmpp.Jid -> Jingle -> TVar State -> TChan a
                 -> IO (Maybe (Xmpp.IQRequestTicket -> Jingle -> IO ())))
            -> (Xmpp.IQRequest -> IO Bool)
            -> Xmpp.Session
            -> IO (Maybe JingleHandler)
startJingle handleContent policy xmppSession = do
    sessions <- newTVarIO Map.empty
    chan' <- Xmpp.listenIQChan Xmpp.Set "urn:xmpp:jingle:1" xmppSession
    case chan' of
        Left _ -> errorM "Jingle" "Jingle channel is already in use"
                  >> return Nothing
        Right chan -> do
            thread <- forkIO . forever $ do
                ticket <- atomically $ readTChan chan
                let request = Xmpp.iqRequestBody ticket
                case unpickleElem xpJingle $ Xmpp.iqRequestPayload request of
                    Left _ -> return ()
                    Right ji -> case action ji of
                        SessionInitiate -> sessionInitiate sessions ji ticket
                        _               -> handleSession sessions ji ticket
            return . Just $ JingleHandler
                               { jingleSessions = sessions
                               , jingleThread = thread
                               }

  where
    sessionInitiate sessions ji ticket = do
        sess <- atomically $ readTVar sessions
        case Map.lookup (sid ji) sess of
            Nothing -> do
                stateRef <- newTVarIO PENDING
                requestsRef <- newTChanIO
                mbs <- newSession ji ticket stateRef requestsRef
                case mbs of
                    Nothing -> return ()
                    Just s -> do
                        atomically . modifyTVar sessions $ Map.insert (sSid s) s
            Just _ -> return () -- TODO
    handleSession sessions ji ticket = do
        sess <- atomically $ readTVar sessions
        case Map.lookup (sid ji) sess of
            Nothing -> checkPolicy ticket $ do return () -- TODO?
            Just session -> if (Xmpp.iqRequestFrom $ Xmpp.iqRequestBody ticket)
                               == (Just $ sRemote session)
                            then if isSessionPing ji then
                                   do
                                       Xmpp.answerIQ ticket
                                                     (Right Nothing)
                                                     xmppSession
                                       return ()
                                 else sRequests session ticket ji
                            else errorUnavailable ticket xmppSession
    checkPolicy ticket f = do
        answer <- policy (Xmpp.iqRequestBody ticket)
        if answer then f else errorUnavailable ticket xmppSession
    newSession ji ticket stateRef requestsRef =
        case ( Xmpp.iqRequestFrom $ Xmpp.iqRequestBody ticket) of
            (Just remote) -> do
                Xmpp.answerIQ ticket (Right Nothing) xmppSession
                handle <- handleContent remote ji stateRef requestsRef
                case handle of
                    Just handlerFunc -> do
                        void $ Xmpp.answerIQ ticket (Right Nothing) xmppSession
                        return . Just $ Session { sState = stateRef
                                , sSid = sid ji
                                , sRemote = remote
                                , sRequests = handlerFunc
                                }
                    Nothing -> return Nothing
            _ -> errorBadRequest ticket xmppSession >> return Nothing
    elementNS = nameNamespace . elementName
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
    if Map.member (sSid s) hs
        then return False
        else do
        writeTVar href $ Map.insert (sSid s) s hs
        return True
