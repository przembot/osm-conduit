{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Data.Conduit.OSM
  (
    sourceFile
  , conduitNWR
  , conduitNodes
  , conduitWays
  , conduitRelations
  , conduitOSM
  )
  where

import Data.Conduit                 (Consumer, Conduit, Source, (=$))
import Data.Text                    (Text, unpack, toLower)
import Data.XML.Types               (Event, Name)
import Control.Monad                (void)
import Control.Monad.Catch          (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Text.XML.Stream.Parse        (AttrParser, tagName, requireAttr, attr
                                    , ignoreAttrs, many, many', manyYield, manyYield'
                                    , parseFile, def, force, choose)
import Data.Conduit.OSM.Types


sourceFile :: MonadResource m => FilePath -> Source m OSM
sourceFile path = parseFile def path =$ conduitOSM

conduitOSM :: MonadThrow m => Conduit Event m OSM
conduitOSM = manyYield parseOSM

conduitNodes :: MonadThrow m => Conduit Event m Node
conduitNodes = force "node can't be parsed" $
                  tagName "osm" ignoreAttrs $ \_ -> void $ manyYield' parseNode

conduitWays :: MonadThrow m => Conduit Event m Way
conduitWays = force "ways can't be parsed" $
                  tagName "osm" ignoreAttrs $ \_ -> void $ manyYield' parseWay

conduitRelations :: MonadThrow m => Conduit Event m Relation
conduitRelations = force "relations can't be parsed" $
                    tagName "osm" ignoreAttrs $ \_ -> void $ manyYield' parseRelation

conduitNWR :: MonadThrow m => Conduit Event m NWRWrap
conduitNWR = force "cannot parse" $
                  tagName "osm" ignoreAttrs $ \_ -> void $ manyYield' parseNWR

parseOSM :: MonadThrow m => Consumer Event m (Maybe OSM)
parseOSM = tagName "osm" tagParser $ \cont -> cont <$> parseBounds <*> many parseNode <*> many parseWay <*> many' parseRelation
  where
    tagParser = OSM <$> requireAttrRead "version" <*> attr "generator" <* ignoreAttrs


-- | Wrap nodes, ways and relations
parseNWR :: MonadThrow m => Consumer Event m (Maybe NWRWrap)
parseNWR = choose [ fmap (fmap N) $ parseNode
                  , fmap (fmap W) $ parseWay
                  , fmap (fmap R) $ parseRelation ]


parseNode :: MonadThrow m => Consumer Event m (Maybe Node)
parseNode = tagName "node" tagParser $ \cont -> cont <$> many' parseTag
  where
    tagParser = (\f latitude longitude tagz -> Node latitude longitude (f tagz))
                <$> nwrCommonParser
                <*> requireAttrRead "lat"
                <*> requireAttrRead "lon"
                <*  ignoreAttrs


parseWay :: MonadThrow m => Consumer Event m (Maybe Way)
parseWay = tagName "way" (nwrCommonParser <* ignoreAttrs)
    $ \cont -> Way <$> many parseNd <*> (cont <$> many' parseTag)


parseRelation :: MonadThrow m => Consumer Event m (Maybe Relation)
parseRelation = tagName "relation" (nwrCommonParser <* ignoreAttrs)
    $ \cont -> Relation <$> many parseMember <*> (cont <$> many' parseTag)


parseMember :: MonadThrow m => Consumer Event m (Maybe Member)
parseMember = tagName "member" tagParser return
  where
    tagParser = Member <$> fmap readNWRType (requireAttr "type")
                       <*> requireAttr "ref"
                       <*> attr "role"
                       <* ignoreAttrs


parseNd :: MonadThrow m => Consumer Event m (Maybe Nd)
parseNd = tagName "nd" (Nd <$> requireAttr "ref" <* ignoreAttrs) return


parseTag :: MonadThrow m => Consumer Event m (Maybe Tag)
parseTag = tagName "tag" tagParser (return . Tag)
  where
    tagParser = (,) <$> requireAttr "k" <*> requireAttr "v" <* ignoreAttrs

parseBounds :: MonadThrow m => Consumer Event m (Maybe Bounds)
parseBounds = tagName "bounds" tagParser return
  where
    tagParser = Bounds <$> requireAttrRead "minlat"
                       <*> requireAttrRead "minlon"
                       <*> requireAttrRead "maxlat"
                       <*> requireAttrRead "maxlon"

nwrCommonParser :: AttrParser ([Tag] -> NWRCommon)
nwrCommonParser =  NWRCommon <$> requireAttr "id" <*> fmap (>>= readBool) (attr "visible") <*> attr "chageset" <*> attr "timestamp" <*> attr "user"


readNWRType :: Text -> NWR
readNWRType a =
  case toLower a of
    "node" -> NWRn
    "relation" -> NWRr
    "way" -> NWRw
    _ -> error "unknown type in <member>"

fromStr :: Read a => Text -> a
fromStr = read . unpack

requireAttrRead :: Read a => Name -> AttrParser a
requireAttrRead str = fmap fromStr (requireAttr str)

readBool :: Text -> Maybe Bool
readBool a
  | toLower a == "true" = Just True
  | toLower a == "false" = Just False
  | otherwise = Nothing
