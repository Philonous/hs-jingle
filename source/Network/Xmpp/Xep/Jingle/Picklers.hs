{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Xep.Jingle.Picklers where

import Data.XML.Pickle
import Data.XML.Types
import Data.Text (Text)
import Network.Xmpp.Xep.Jingle.Types

ns :: Text
ns = "urn:xmpp:jingle:1"

jName n = Name n (Just ns) Nothing

errorNS :: Text
errorNS = "urn:xmpp:jingle:errors:1"

jeName n = Name n (Just errorNS) Nothing

infixl 1 $.
($.) = id

xpJingleContent :: PU [Node] [JingleContent]
xpJingleContent = xpWrap
    (map $ \((c, d, n, s), (de, tr, se)) -> JingleContent c d n s de tr se)
    (map $ \(JingleContent c d n s de tr se) -> ((c, d, n, s), (de, tr, se)))
    $ xpElems (jName "content")
      (xp4Tuple
         $. xpAttr "creator" xpPrim
         $. xpAttrImplied "disposition" xpText
         $. xpAttr "name" xpText
         $. xpAttrImplied "senders" xpPrim
      )
      (xp3Tuple xpDescription xpTransport xpSecurity)


-- xpJingleReason = xpOption $ xpElemNodes (jName "reason") $
--                    xpElemWithname

reasonSelector (AlternativeSession _)  =  0
reasonSelector Busy                    =  1
reasonSelector Cancel                  =  2
reasonSelector ConnectivityError       =  3
reasonSelector Decline                 =  4
reasonSelector Expired                 =  5
reasonSelector FailedApplication       =  6
reasonSelector FailedTransport         =  7
reasonSelector GeneralError            =  8
reasonSelector Gone                    =  9
reasonSelector IncompatibleParameters  = 10
reasonSelector MediaError              = 11
reasonSelector SecurityError           = 12
reasonSelector Success                 = 13
reasonSelector Timeout                 = 14
reasonSelector UnsupportedApplications = 15
reasonSelector UnsupportedTransports   = 16

xpReasons :: [PU [Node] ReasonType]
xpReasons = (xpWrap (\sid -> AlternativeSession sid)
                    (\(AlternativeSession sid) -> sid) $
                  xpElemNodes (jName "alternative-session") $
                      xpOption $ xpElemNodes (jName "sid") (xpContent xpText))
            : (map (\(con, name) -> xpConst con $ xpElemBlank (jName name) ) $
                [ (Busy                   , "busy"                    )
                , (Cancel                 , "cancel"                  )
                , (ConnectivityError      , "connectivity-error"      )
                , (Decline                , "decline"                 )
                , (Expired                , "expired"                 )
                , (FailedApplication      , "failed-application"      )
                , (FailedTransport        , "failed-transport"        )
                , (GeneralError           , "general-error"           )
                , (Gone                   , "gone"                    )
                , (IncompatibleParameters , "incompatible-parameters" )
                , (MediaError             , "media-error"             )
                , (SecurityError          , "security-error"          )
                , (Success                , "success"                 )
                , (Timeout                , "timeout"                 )
                , (UnsupportedApplications, "unsupported-applications")
                , (UnsupportedTransports  , "unsupported-transports"  )
                ])

xpJingleReason :: PU [Node] (Maybe JingleReason)
xpJingleReason = xpOption $ xpWrap
                   (\(rt, t, p) -> JingleReason rt t p)
                   (\(JingleReason rt t p) -> (rt, t, p)) $
                xpElemNodes "reason" $
                 xp3Tuple
                     $. xpAlt reasonSelector xpReasons
                     $. xpOption (xpElemText (jName "text"))
                     $. xpOption (xpIsolate (xpElemVerbatim))

xpJingle = xpWrap (\((a, i, r, s), (con, rea, els)) -> Jingle a i r s con els rea)
                  (\(Jingle a i r s con els rea) -> ((a, i, r, s), (con, rea, els)))
                  $
    xpElem (jName "jingle")
             (xp4Tuple
                 $. xpAttr "action" xpPrim
                 $. xpAttrImplied "initiator" xpPrim
                 $. xpAttrImplied "responder" xpPrim
                 $. xpAttr "sid" xpText
             )
             (xp3Tuple
                xpJingleContent
                xpJingleReason
                (xpFindMatches xpElemVerbatim)
             )

unpickleElem :: PU [Node] a -> Element -> Either UnpickleError a
unpickleElem p x = unpickle (xpRoot . xpUnliftElems $  p) x

pickleElem :: PU [Node] a -> a -> Element
pickleElem p x = pickle (xpRoot . xpUnliftElems $  p) x

xpElementByLocalName :: Text -> PU [Node] Element -> PU [Node] Element
xpElementByLocalName ln xp = xpFindFirst isTransport xp
  where
    isTransport (NodeElement (Element name _ _ )) = nameLocalName name == ln
    isTransport _ = False

xpTransport = xpOption $ xpElementByLocalName "transport" xpElemVerbatim

xpDescription = xpOption $ xpElementByLocalName "description" xpElemVerbatim

xpSecurity = xpOption $ xpElementByLocalName "security" xpElemVerbatim
