{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (catch)

import Hermitage.YesodNode
import Hermitage.VerifierNode
import Hermitage.Roles
import Hermitage.MessageTypes

import Remote
import Remote.Process

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import Text.Printf

import Data.Global
import Data.IORef
import qualified Data.Map

import System.Exit

nodeRole :: IORef String
nodeRole = declareIORef "hermitage::nodeRole" (error "role not yet configured")

withMonitorInitial proc = do
  nid <- getSelfNode
  selfPid <- getSelfPid
  pid <- nameQuery nid "initialProcess"
  case pid of
    Nothing -> say "Error: no initialProcess. Aborting."
    Just pid' -> do
             monitorProcess selfPid pid' MaLink
             proc
             unmonitorProcess selfPid pid' MaLink

foreverLogErrors :: ProcessM () -> ProcessM ()
foreverLogErrors f = do
   res <- ptry f
   case res of
     Right _ -> return ()
     Left e -> logS "HER" LoCritical (show (e :: SomeException)) >> (liftIO $ threadDelay (10^6))
   foreverLogErrors f

-- | run when the node is not properly configured, i.e. has 'role' of a 'NODE'
unconfiguredNode = do
  say "This node is not properly configured. The default role of NODE is invalid. It will exit now."
  shutdownSelf

-- | configured role is unkown
unknownRole role = do
  say (printf "This node is not properly configured. The configured role of %s is unknown and thus invalid. It will exit now." role)
  shutdownSelf

-- | stop this node
shutdownSelf :: ProcessM ()
shutdownSelf = shutdownNode =<< getSelfNode

-- | try stopping provided node
shutdownNode :: NodeId -> ProcessM ()
shutdownNode nid = do
  pid <- nameQuery nid "initialProcess"
  case pid of
    Nothing -> say "shutdownNode: error: no initialProcess on node"
    Just pid' -> do
             status <- sendQuiet pid' Shutdown
             say (printf "shutdownNode:%s" (show status))

-- | coordinator runs task based on configured role and restarts when it finishes
runCoordinator :: ProcessM ()
runCoordinator = do
  setDaemonic 
  foreverLogErrors $ do
    role <- liftIO (readIORef nodeRole)

    case role of
      _ | role == Hermitage.Roles.role_verifier -> runVerifier
        | role == Hermitage.Roles.role_yesod -> runYesodProc
        | role == Hermitage.Roles.role_commander -> runCommander
        | role == Hermitage.Roles.role_unconfigured -> unconfiguredNode
        | otherwise -> unknownRole role

    say ("Worker has finished for some reason. Waiting 3 seconds before restarting...")
    liftIO $ threadDelay ((10^6) * 3)

runCommander :: ProcessM ()
runCommander = do
  setDaemonic

  let cmd'shutdown = do
        say "Requesting shutdown on all nodes..." 
        ps <- getPeers
        forM (Data.Map.toList ps) (\ (role,nodes) -> when (role /= Hermitage.Roles.role_commander) $ do
                                                  say ("ROLE: " ++ show role ++ " NODES: " ++ show nodes)
                                                  mapM_ shutdownNode nodes)
        return ()
      cmd'unknown ln = do
        say $ "Unknown command: " ++ show ln
        say $ "Valid commands: quit shutdown"

  foreverLogErrors $ do
       say "Waiting for command."
       ln <- liftIO getLine
       case ln of
         "quit" -> shutdownSelf
         "shutdown" -> cmd'shutdown    
         _ -> cmd'unknown ln

-- | initial process. keeps the system running (isn't daemonic proc), awaits 'Shutdown' message
initialProcess :: String -> ProcessM ()
initialProcess role = do
  -- first things first: remember our role
  liftIO $ writeIORef nodeRole role
  nameSet "initialProcess"
  spawnLocal runCoordinator
  forever (receiveWait [match (\ Shutdown -> say "Shutting down..." >> terminate)
                       ,matchUnknown (say "Unknown message received, ignoring...")])


-- | main function. should be placed at the bottom of file due to TemplateHaskell issues.
main :: IO ()
main = remoteInit Nothing [] initialProcess
