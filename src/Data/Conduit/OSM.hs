{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Data.Conduit.OSM
  (
    sourceFile
  , conduitOSM
  , conduitNodes
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
                                    , parseFile, def, force)
import Data.Conduit.OSM.Types


sourceFile :: MonadResource m => FilePath -> Source m OSM
sourceFile path = parseFile def path =$ conduitOSM

conduitOSM :: MonadThrow m => Conduit Event m OSM
conduitOSM = manyYield parseOSM

conduitNodes :: MonadThrow m => Conduit Event m Node
conduitNodes = force "node can't be parsed" $
                  tagName "osm" ignoreAttrs $ \_ -> void $ manyYield' parseNode

parseOSM :: MonadThrow m => Consumer Event m (Maybe OSM)
parseOSM = tagName "osm" tagParser $ \cont -> cont <$> parseBounds <*> many parseNode <*> many parseWay <*> many' parseRelation
  where
    tagParser = OSM <$> requireAttrRead "version" <*> attr "generator" <* ignoreAttrs


parseNode :: MonadThrow m => Consumer Event m (Maybe Node)
parseNode = tagName "node" tagParser $ \cont -> cont <$> many' parseTag
  where
    tagParser = (\la lo i vis chset tstamp usr tgs ->
                    Node la lo (NWRCommon i vis chset tstamp usr tgs))
                <$> requireAttrRead "lat"
                <*> requireAttrRead "lon"
                <*> requireAttr "id"
                <*> fmap (>>= readBool) (attr "visible")
                <*> attr "chageset"
                <*> attr "timestamp"
                <*> attr "user"
                <* ignoreAttrs


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
    "node" -> N
    "relation" -> R
    "way" -> W
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