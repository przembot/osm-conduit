{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import Data.Text (Text)
import Data.List (any)

import Text.XML.Stream.Parse (parseFile, def)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.OSM
import Data.Conduit.OSM.Types

import Control.Lens ((^.))
import System.Environment (getArgs)

conduitAlko :: MonadThrow m => Conduit Node m Node
conduitAlko = CL.filter ( any ((== ("shop", "alcohol")) . _tag)  . (^. nCommon . tags))

sinkAlko :: (MonadThrow m, MonadIO m) => Sink Node m ()
sinkAlko = awaitForever $ liftIO . print

getAlco :: FilePath -> IO ()
getAlco fpath = runResourceT $ parseFile def fpath =$ conduitNodes =$ conduitAlko $$ sinkAlko


-- Get all alcohol shops found in map file
main :: IO ()
main = do
  [path] <- getArgs
  getAlco path
