{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

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

import System.Process
import System.Random
import System.FilePath
import System.Directory

import Control.Exception
import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.String
import Text.Printf

data VerifierException = BadLanguage {msg :: String} deriving (Show, Typeable)
instance Exception VerifierException

runVerifier :: ProcessM ()
runVerifier = do
  say "Verifier starting"
  setDaemonic

  tmpdir' <- liftIO $ replicateM 10 (randomRIO ('a','z'))
  let tmpdir = "var" </> "tmp" </> tmpdir'
  say $ "Creating tmp directory " ++ show tmpdir
  liftIO $ createDirectoryIfMissing True tmpdir

  finished <- liftIO newEmptyMVar
  poolVar <- liftIO newEmptyMVar

  liftIO $ forkIO $ runDBBackend $ \pool -> do
                putMVar poolVar pool
                takeMVar finished

  loggerVar <- liftIO newEmptyMVar

  let handle'haskell val = do
         let file = (tmpdir </> "script.hs")
         writeFile file (submissionText val)
         output <- try (readProcess "runhaskell" [file] "")
         return output
      handle'bash val = do
         let file = (tmpdir </> "script.sh")
         writeFile file (submissionText val)
         output <- try (readProcess "sh" [file] "")
         return output

  let watchDB pool = liftIO $ do
                entities <- runSqlPool (do
                                           subm <- selectFirst [SubmissionStatus ==. "fresh"] []
                                           case subm of
                                             Nothing -> return ()
                                             Just ent -> do
                                                       update (entityKey ent) [SubmissionStatus =. "processing"]
                                           return subm
                                         ) pool
                case entities of
                  Nothing -> threadDelay (10^6) -- nothing to do
                  Just ent -> do
                               say' ("Executing job: " ++ (show ent))
                               let val = entityVal ent
                               res <- case submissionLanguage val of
                                 "haskell" -> handle'haskell val
                                 "bash" -> handle'bash val
                                 lang -> return (Left $ toException (BadLanguage ("Unknown submission language: " ++ show lang)))
                               say' ("Execution finished. Result: " ++ show res)
                               runSqlPool (update (entityKey ent) [SubmissionStatus =. (fromString $ printf "finished (%s)" (show res))]) pool

      say' what = putMVar loggerVar what
      logger = forever $ do
                 what <- liftIO $ takeMVar loggerVar
                 say what

  spawnLocal $ setDaemonic >> logger

  loopPid <- spawnLocal $ do
    setDaemonic
    forever $ do
      say "Looking for jobs to do..."
      pbracket (liftIO $ takeMVar poolVar) (\pool -> liftIO (putMVar poolVar pool)) watchDB
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
