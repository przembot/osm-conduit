-- |
-- The following data types correspond to documentation of API version 0.6.
-- See https://wiki.openstreetmap.org/wiki/API_v0.6/DTD
module Data.Conduit.OSM.Types where

import Data.Text (Text)


type Version = Double
type Generator = Text

data OSM =
  OSM {
      _version   :: Version
    , _generator :: Maybe Generator
    , _bounds    :: Maybe Bounds
    , _nodes     :: [Node]
    , _ways      :: [Way]
    , _relations :: [Relation]
    }
    deriving (Show, Eq)

data Bounds =
  Bounds {
      _minlat :: Lat
    , _minlon :: Lon
    , _maxlat :: Lat
    , _maxlon :: Lon
    }
    deriving (Show, Eq)

type Id = Text
type Lat = Double
type Lon = Double
type Changeset = Text
type Visible = Bool
type User = Text
type Timestamp = Text -- maybe data.time?
type Role = Text

data Node =
  Node {
    _lat :: Lat
  , _lon :: Lon
  , _nCommon :: NWRCommon
  }
  deriving (Show, Eq)

data Relation =
  Relation {
    _members :: [Member]
  , _rCommon :: NWRCommon
  }
  deriving (Show, Eq)

data Way =
  Way {
    _nds :: [Nd]
  , _wCommon :: NWRCommon
  }
  deriving (Show, Eq)

data NWRCommon =
  NWRCommon {
    _id :: Id
  , _visible :: Maybe Visible
  , _changeset :: Maybe Changeset
  , _timestamp :: Maybe Timestamp
  , _user :: Maybe User
  , _tags :: [Tag]
  }
  deriving (Show, Eq)

data NWR = NWRn | NWRw | NWRr
  deriving (Show, Eq)

data NWRWrap = N Node
             | W Way
             | R Relation
  deriving (Show, Eq)

data Member =
  Member {
    _type :: NWR
  , _mRef :: Id
  , _role :: Maybe Role
  }
  deriving (Show, Eq)


data Nd =
  Nd {
    _ref :: Id
  }
  deriving (Show, Eq)

-- | Tag stores (Key, Value)
data Tag =
  Tag {
      _tag :: (Text, Text)
  }
  deriving (Show, Eq)

