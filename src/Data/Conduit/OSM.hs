{-# LANGUAGE OverloadedStrings #-}
-- |
-- Available conduit combinators to process data from *.osm file.
-- For the best performance, use any of conduitNodes/Ways/Relations/NWR.
-- Example:
--
-- > import qualified Data.Conduit.List as CL
-- > import Text.XML.Stream.Parse (parseFile, def)
-- > printNodes filepath = parseFile def filepath =$ conduitNodes $$ CL.mapM_ print
--

module Data.Conduit.OSM
  (
    sourceFileOSM
  , conduitNWR
  , conduitNodes
  , conduitWays
  , conduitRelations
  , conduitOSM
  )
  where

import Data.Conduit                 (Consumer, Conduit, Source, ConduitM, (=$))
import Data.Text                    (Text, unpack, toLower)
import Data.XML.Types               (Event, Name)
import Control.Monad.Catch          (MonadThrow, throwM)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception            (ErrorCall(..))
import Text.Read                    (readMaybe)
import Text.XML.Stream.Parse        (AttrParser, tagName, requireAttr, attr
                                    , ignoreAttrs, many, many', manyYield, manyYield'
                                    , parseFile, def, choose, tagIgnoreAttrs)
import Data.Conduit.OSM.Types



sourceFileOSM :: MonadResource m => FilePath -> Source m OSM
sourceFileOSM path = parseFile def path =$ conduitOSM

conduitOSM :: MonadThrow m => Conduit Event m OSM
conduitOSM = manyYield parseOSM

conduitNodes :: MonadThrow m => Conduit Event m Node
conduitNodes = loopConduit $ tagIgnoreAttrs "osm" $ manyYield' parseNode

conduitWays :: MonadThrow m => Conduit Event m Way
conduitWays = loopConduit $ tagIgnoreAttrs "osm" $ manyYield' parseWay

conduitRelations :: MonadThrow m => Conduit Event m Relation
conduitRelations = loopConduit $ tagIgnoreAttrs "osm" $ manyYield' parseRelation

conduitNWR :: MonadThrow m => Conduit Event m NWRWrap
conduitNWR = loopConduit $ tagIgnoreAttrs "osm" $ manyYield' parseNWR

-- | Keep yielding output if parser can still parse anything remaining
loopConduit :: Monad m => ConduitM i o m (Maybe ()) -> Conduit i m o
loopConduit cond = loop
  where
    loop = cond >>= maybe (return ()) (const loop)

parseOSM :: MonadThrow m => Consumer Event m (Maybe OSM)
parseOSM = tagName "osm" tagParser $ \cont -> cont <$> parseBounds <*> many parseNode <*> many parseWay <*> many' parseRelation
  where
    tagParser = OSM <$> requireAttrRead "version" <*> attr "generator" <* ignoreAttrs


-- | Wrap nodes, ways and relations
parseNWR :: MonadThrow m => Consumer Event m (Maybe NWRWrap)
parseNWR = choose [ fmap N <$> parseNode
                  , fmap W <$> parseWay
                  , fmap R <$> parseRelation ]


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
    tagParser = Member <$> (requireAttr "type" >>= readNWRType)
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
nwrCommonParser = NWRCommon <$> requireAttr "id"
                             <*> fmap (>>= readBool) (attr "visible")
                             <*> attr "chageset"
                             <*> attr "timestamp"
                             <*> attr "user"

readNWRType :: Text -> AttrParser NWR
readNWRType a =
  case toLower a of
    "node" -> return NWRn
    "relation" -> return NWRr
    "way" -> return NWRw
    _ -> throwM $ ErrorCall "unknown type in <member>"

fromStr :: Read a => Text -> Maybe a
fromStr = readMaybe . unpack

requireAttrRead :: Read a => Name -> AttrParser a
requireAttrRead str = requireAttr str
          >>= maybe (throwM $ ErrorCall "Could not parse attribute value") return . fromStr

readBool :: Text -> Maybe Bool
readBool a
  | toLower a == "true" = Just True
  | toLower a == "false" = Just False
  | otherwise = Nothing
