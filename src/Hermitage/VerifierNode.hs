{-# LANGUAGE StandaloneDeriving, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

module Hermitage.VerifierNode where

import Remote
import Remote.Process

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Database.Persist.Postgresql
import Database.Persist.Sqlite
  
import Hermitage.YesodNode (runDBBackend)
import Hermitage.DbTypes

runVerifier :: ProcessM ()
runVerifier = do
  say "Verifier starting"
  setDaemonic

  finished <- liftIO newEmptyMVar
  poolVar <- liftIO newEmptyMVar

  liftIO $ forkIO $ runDBBackend $ \pool -> do
                putMVar poolVar pool
                takeMVar finished

  let doWork pool = liftIO $ do
                entities <- runSqlPool (do
                                           subm <- selectFirst [SubmissionStatus ==. "fresh"] []
                                           case subm of
                                             Nothing -> return ()
                                             Just ent -> do
                                                       update (entityKey ent) [SubmissionStatus =. "processing"]
                                           return subm
                                         ) pool
                when (entities /= Nothing) (print entities)

  loopPid <- spawnLocal $ do
    setDaemonic
    forever $ do
      say "Looking for jobs to do..."
      pbracket (liftIO $ takeMVar poolVar) (\pool -> liftIO (putMVar poolVar pool)) doWork
      liftIO (threadDelay (10^5))


  selfPid <- getSelfPid
  monitorProcess selfPid loopPid MaMonitor

  say "Waiting for loop process to finish..."
  msg <- expect
  say $ show (msg :: ProcessMonitorException)
  liftIO $ putMVar finished ()

  -- slight delay to let runSqlPool finish... quite frankly: a hack.
  liftIO $ threadDelay (10^6)
  say "Verifier finished"

-- $( remotable ['runVerifier] )
