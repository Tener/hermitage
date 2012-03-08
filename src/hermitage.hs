{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Remote.Monitoring -- ekg

import Control.Concurrent
import Data.ByteString.Char8 ()
import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config

import Datatypes
import Globals
import Handlers (myServe)

main = do
  initConnectionPool
  migrateAllDatatypes

  insertTestData

  forkServer "localhost" 7000
  forkIO (quickHttpServe myServe)

  mapM_ (\ i -> print i >> threadDelay (10^8)) [1..]

  