{-# LANGUAGE BangPatterns #-}
module Network.Xmpp.Xep.Jingle.Types where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.STM
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.XML.Types
import           Network.Socket
import qualified Network.Xmpp as Xmpp

data State = PENDING | ACTIVE | ENDED deriving Show

data Action = ContentAccept
            | ContentAdd
            | ContentModify
            | ContentReject
            | ContentRemove
            | DescriptionInfo
            | SecurityInfo
            | SessionAccept
            | SessionInfo
            | SessionInitiate
            | SessionTerminate
            | TransportAccept
            | TransportInfo
            | TransportReject
            | TransportReplace


instance Show Action where
    show ContentAccept    = "content-accept"
    show ContentAdd       = "content-add"
    show ContentModify    = "content-modify"
    show ContentReject    = "content-reject"
    show ContentRemove    = "content-remove"
    show DescriptionInfo  = "description-info"
    show SecurityInfo     = "security-info"
    show SessionAccept    = "session-accept"
    show SessionInfo      = "session-info"
    show SessionInitiate  = "session-initiate"
    show SessionTerminate = "session-terminate"
    show TransportAccept  = "transport-accept"
    show TransportInfo    = "transport-info"
    show TransportReject  = "transport-reject"
    show TransportReplace = "transport-replace"

instance Read Action where
    readsPrec _ "content-accept"    = [(ContentAccept    , "")]
    readsPrec _ "content-add"       = [(ContentAdd       , "")]
    readsPrec _ "content-modify"    = [(ContentModify    , "")]
    readsPrec _ "content-reject"    = [(ContentReject    , "")]
    readsPrec _ "content-remove"    = [(ContentRemove    , "")]
    readsPrec _ "description-info"  = [(DescriptionInfo  , "")]
    readsPrec _ "security-info"     = [(SecurityInfo     , "")]
    readsPrec _ "session-accept"    = [(SessionAccept    , "")]
    readsPrec _ "session-info"      = [(SessionInfo      , "")]
    readsPrec _ "session-initiate"  = [(SessionInitiate  , "")]
    readsPrec _ "session-terminate" = [(SessionTerminate , "")]
    readsPrec _ "transport-accept"  = [(TransportAccept  , "")]
    readsPrec _ "transport-info"    = [(TransportInfo    , "")]
    readsPrec _ "transport-reject"  = [(TransportReject  , "")]
    readsPrec _ "transport-replace" = [(TransportReplace , "")]
    readsPrec _ _                   = []


data Jingle = Jingle { action :: !Action
                     , initiator :: !(Maybe Xmpp.Jid)
                     , responder :: !(Maybe Xmpp.Jid)
                     , sid :: !Text
                     , content :: [JingleContent]
                     , jinglePayload :: [Element]
                     , reason :: !(Maybe JingleReason)
                     } deriving Show


data Creator = CInitiator | CResponder

instance Show Creator where
    show CInitiator = "initiator"
    show CResponder = "responder"

instance Read Creator where
    readsPrec _ "initiator" = [(CInitiator, "")]
    readsPrec _ "responder" = [(CResponder, "")]
    readsPrec _ _ = []

data Senders = SNone | SInitiator | SResponder | SBoth

instance Show Senders where
    show SNone      = "none"
    show SInitiator = "initiator"
    show SResponder = "responder"
    show SBoth      = "both"

instance Read Senders where
    readsPrec _ "none"      = [(SNone      , "")]
    readsPrec _ "initiator" = [(SInitiator , "")]
    readsPrec _ "responder" = [(SResponder , "")]
    readsPrec _ "both"      = [(SBoth      , "")]
    readsPrec _ _           = []

data JingleContent = JingleContent { creator :: !Creator
                                   , disposition :: !(Maybe Text)
                                   , name :: !Text
                                   , senders  :: !(Maybe Senders)
                                   , contentDescription :: Maybe Element
                                   , contentTransport :: Maybe Element
                                   , contentSecurity :: Maybe Element
                                   } deriving Show


data JingleReason = JingleReason { reasonType :: !ReasonType
                                 , reasonText :: !(Maybe Text)
                                 , reasonElement :: !(Maybe Element)
                                 } deriving Show

data ReasonType = Busy
                | Cancel
                | ConnectivityError
                | Decline
                | Expired
                | FailedApplication
                | FailedTransport
                | GeneralError
                | Gone
                | IncompatibleParameters
                | MediaError
                | SecurityError
                | Success
                | Timeout
                | UnsupportedApplications
                | UnsupportedTransports
                | AlternativeSession !(Maybe Text)
                deriving (Show, Read, Eq)

    -- <xs:element name='alternative-session'
    --                 type='alternativeSessionElementType'/>
    --     busy                     Busy
    --     cancel                   Cancel
    --     connectivity-error       ConnectivityError
    --     decline                  Decline
    --     expired                  Expired
    --     failed-application       FailedApplication
    --     failed-transport         FailedTransport
    --     general-error            GeneralError
    --     gone                     Gone
    --     incompatible-parameters  Incompatible-parameters
    --     media-error              MediaError
    --     security-error           SecurityError
    --     success                  Success
    --     timeout                  Timeout
    --     unsupported-applications UnsupportedApplications
    --     unsupported-transports   UnsupportedTransports

type MessageChan = TChan (Xmpp.IQRequestTicket, Jingle)

data JingleHandler = JingleHandler
    { jingleSessions :: !(TVar (Map Text Session))
    , jingleThread :: !ThreadId
    , jingleXmppSession :: !Xmpp.Session
    }


data Session = Session { sState  :: TVar State
                       , sSid    :: !Text
                       , sRemote :: !Xmpp.Jid
                       , sRequests :: (Session
                                       -> Xmpp.IQRequestTicket
                                       -> Jingle
                                       -> IO ())
                       }

data TransportType = Datagram | Stream | Both deriving (Eq, Show)

data ApplicationHandler = ApplicationHandler { chTransportType :: TransportType
                                             , chNamespace     :: Text
                                             , chHandler       :: Socket -> IO ()
                                             }

data TransportHandler = TransportHandler { thNamespace     :: Text
                                         , thTransportType :: TransportType
                                         , thHandler :: Xmpp.Session
                                                     -> MessageChan
                                                     -> IO Socket
                                         }

data SecurityHandler = SecurityHandler
