{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (runResourceT)

import Data.Text (Text)

import Data.XML.Types (Event)
import Text.XML.Stream.Parse hiding (force)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.OSM
import Data.Conduit.OSM.Types

main :: IO ()
main = hspec $ do
  describe "parser success" $ do
    it "parse node with tags" $ do
      parsed <- runTest testCase01 conduitOSM
      parsed `shouldBe` [OSM 0.6 (Just "lulz generator") Nothing [Node 52.153 22.341 (NWRCommon "43221" (Just True) Nothing Nothing Nothing [Tag ("shop", "alcohol"), Tag ("area", "safe")])] [] []]

    it "parse way" $ do
      parsed <- runTest testCase03 conduitOSM
      parsed `shouldBe` [OSM 0.6 (Just "lulz generator") Nothing [] [Way [Nd "1234", Nd "2345", Nd "3456"] (NWRCommon "1995" (Just True) Nothing Nothing Nothing [Tag ("rodzaj drogi", "do ukochanej")]) ] [] ]

    it "parse relation" $ do
      parsed <- runTest testCase04 conduitOSM
      parsed `shouldBe` [OSM 0.6 (Just "lulz generator") Nothing [] [] [Relation [Member NWRn "1234" Nothing] (NWRCommon "4747" (Just True) Nothing Nothing Nothing [Tag ("testk", "testv")])]]

  describe "parser throws error" $ do
    it "should throw when reading a double value fails" $ do
     parsed <- runTest testCase02 conduitOSM
     (evaluate . force) (show parsed) `shouldThrow` errorCall "Prelude.read: no parse"

  describe "reading from file" $ do
    it "should be able to read using sourceFile" $ do
      parsed <- runResourceT $ sourceFile "test/readingTest.xml" $$ CL.consume
      parsed `shouldBe` [OSM 0.6 Nothing Nothing [Node 12.34 34.56 (NWRCommon "1111" Nothing Nothing Nothing Nothing [])] [] []]

  describe "parsing ommiting osm tag" $ do
    it "parse nwr at once" $ do
      parsed <- runTest testCase05 conduitNWR
      parsed `shouldMatchList` [
        N (Node 52.153 22.341 (NWRCommon "43221" (Just True) Nothing Nothing Nothing [])),
        W (Way [Nd "12"] (NWRCommon "1337" (Just True) Nothing Nothing Nothing [])),
        R (Relation [Member NWRn "1234" Nothing] (NWRCommon "4747" (Just True) Nothing Nothing Nothing [Tag ("testk", "testv")]))
        ]

    it "parse nodes using conduitNodes" $ do
      parsed <- runTest testCase01 conduitNodes
      parsed `shouldBe` [Node 52.153 22.341 (NWRCommon "43221" (Just True) Nothing Nothing Nothing [Tag ("shop", "alcohol"), Tag ("area", "safe")])]

    it "parse ways using conduitWays" $ do
      parsed <- runTest testCase03 conduitWays
      parsed `shouldBe` [Way [Nd "1234", Nd "2345", Nd "3456"] (NWRCommon "1995" (Just True) Nothing Nothing Nothing [Tag ("rodzaj drogi", "do ukochanej")])]

    it "parse relations using conduitRelations" $ do
      parsed <- runTest testCase04 conduitRelations
      parsed `shouldBe` [Relation [Member NWRn "1234" Nothing] (NWRCommon "4747" (Just True) Nothing Nothing Nothing [Tag ("testk", "testv")])]

    it "parse items from many osm tags" $ do
      parsed <- runTest testCase06 conduitNWR
      parsed `shouldMatchList` [
        N (Node 52 20 (NWRCommon "21" (Just True) Nothing Nothing Nothing [])),
        N (Node 52.153 22.341 (NWRCommon "43221" (Just True) Nothing Nothing Nothing []))
        ]

runTest :: MonadThrow m => Source m Text -> Conduit Event m a -> m [a]
runTest tcase cond = tcase $$ parseText' def =$ cond =$ CL.consume

xmlPrefix :: Text
xmlPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

testCase01 :: Monad m => Source m Text
testCase01 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<node id=\"43221\" lat=\"52.153\" lon=\"22.341\" visible=\"true\">"
      , "<tag k=\"shop\" v=\"alcohol\" />"
      , "<tag k=\"area\" v=\"safe\" />"
      , "</node>"
      , "</osm>"
      ]

testCase02 :: Monad m => Source m Text
testCase02 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<node id=\"43221\" lat=\"52.ad153\" lon=\"22.341\" visible=\"true\">"
      , "<tag k=\"shop\" v=\"alcohol\" />"
      , "</node>"
      , "</osm>"
      ]

testCase03 :: Monad m => Source m Text
testCase03 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<way id=\"1995\" visible=\"true\">"
      , "<nd ref=\"1234\" />"
      , "<nd ref=\"2345\" />"
      , "<nd ref=\"3456\" />"
      , "<tag k=\"rodzaj drogi\" v=\"do ukochanej\" />"
      , "</way>"
      , "</osm>"
      ]

testCase04 :: Monad m => Source m Text
testCase04 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<relation id=\"4747\" visible=\"true\">"
      , "<member type=\"node\" ref=\"1234\" />"
      , "<tag k=\"testk\" v=\"testv\" />"
      , "</relation>"
      , "</osm>"
      ]

testCase05 :: Monad m => Source m Text
testCase05 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<relation id=\"4747\" visible=\"true\">"
      , "<member type=\"node\" ref=\"1234\" />"
      , "<tag k=\"testk\" v=\"testv\" />"
      , "</relation>"
      , "<node id=\"43221\" lat=\"52.153\" lon=\"22.341\" visible=\"true\" />"
      , "<way id=\"1337\" visible=\"true\">"
      , "<nd ref=\"12\" />"
      , "</way>"
      , "</osm>"
      ]

testCase06 :: Monad m => Source m Text
testCase06 = CL.sourceList
        [xmlPrefix
      , "<osm version=\"0.6\" generator=\"lulz generator\">"
      , "<node id=\"21\" lat=\"52\" lon=\"20\" visible=\"true\" />"
      , "</osm>"
      , "<osm version=\"0.6\" generator=\"phun generator\">"
      , "<node id=\"43221\" lat=\"52.153\" lon=\"22.341\" visible=\"true\" />"
      , "</osm>"
      ]
