{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Xep.Jingle.Test where

import           Data.Either
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text.Lazy (fromChunks)
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp.Xep.Jingle
import           Network.Xmpp.Xep.Jingle.Types
import           Text.XML.Unresolved

import           Network.Xmpp.Xep.Jingle.Types
import           Network.Xmpp.Xep.Jingle

toElement :: Text.Text -> Element
toElement x = case parseText def (fromChunks [x]) of
    Left e -> error $ show e
    Right d -> documentRoot d

testJingle :: Text.Text -> Either UnpickleError Jingle
testJingle x = unpickle xpJingle [NodeElement $ toElement x]

test filename = do
    bs <- Text.readFile $ "tests/" ++ filename
    return $ testJingle bs

filenames = [ "example1.txt"
            , "example2.txt"
            , "example3.txt"
            , "example4.txt"
            , "example6.txt"
            , "example8.txt"
            ]

isRight (Left _) = False
isRight (Right _) = True

tests = mapM (fmap isRight . test) filenames
