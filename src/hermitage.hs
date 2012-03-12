{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hermitage.YesodNode
import Hermitage.Roles
import Hermitage.Types

import Remote
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Global.Variables
import Data.IORef

greet :: String -> ProcessM ()
greet name = say ("Hello, "++name)
badFib :: Integer -> Integer
badFib 0 = 1
badFib 1 = 1
badFib n = badFib (n-1) + badFib (n-2)
$( remotable ['greet, 'badFib] )

nodeRole :: IORef String
nodeRole = declareIORef "hermitage::nodeRole" (error "role not yet configured")

main :: IO ()
main = remoteInit Nothing [Main.__remoteCallMetaData, Hermitage.YesodNode.__remoteCallMetaData] initialProcess

foreverLogErrors f = do
   mvar <- newEmptyMVar
   -- we need to spawn, because we need each invocation to have a different process id. otherwise monitor won't work.
   spawn (catch (finally f (putMVar mvar ())) (\e -> logS "HER" LoCritical (show (e :: SomeException))))
   takeMVar mvar
   foreverLogErrors f

-- | koordynator nadzoruje uruchamia odpowiedni proces roboczy w zależności od aktualnej roli node'a.
runCoordinator :: ProcessM ()
runCoordinator = foreverLogErrors $ do
                   role <- readIORef nodeRole
                   -- zwykły node, uruchom sprawdzaczkę
                   when (role == Hermitage.Roles.role_node) (return ())
                   -- serwer www.
                   when (role == Hermitage.Roles.role_yesod) runYesodProc


-- -- | uruchom master node, tj. node... cholera, po co nam właściwie ten master node?!
-- runMasterNode :: ProcessM ()
-- runMasterNode = foreverLogErrors $ do
--   selfPid <- getSelfPid
--   peers <- getPeers
--   mapM_ (\ node -> nameQueryOrStart node "coordinator" peers runCoordinator__closure ) peers

-- | spróbuj zatrzymać docelowy węzeł. musi mieć uruchomiony initial process.
shutdownNode :: NodeId -> ProcessM ()
shutdownNode nid = do
  pid <- nameQuery nid "initialProcess"
  case pid of
    Nothing -> say "shutdownNode: error: no initialProcess on node"
    Just pid' -> sendQuiet pid' Shutdown

-- | proces startowy, nie robi praktycznie nic poza oczekiwaniem na rozkaz zatrzymania systemu.
initialProcess :: String -> ProcessM ()
initialProcess role = do
  -- pierwsza rzecz tuż po starcie: zapamiętujemy rolę
  writeIORef nodeRole role
  nameSet "initialProcess"
  selfNode <- getSelfNode
  nameQueryOrStart selfNode "coordinator" runCoordinator__closure
  forever (receiveWait [matchUnknown (say "Unknown message received, ignoring..."), 
                        match (\ Shutdown -> say "Shutting down..." >> terminate)])

---  when (role == Hermitage.Roles.role_master) (spawnLocal runMasterNode >> return ())


--  
--   selfPid <- getSelfPid
--  
--  
--   selfNode <- getSelfNode
--   when (role == Hermitage.Roles.role_yesod) (spawnLocal runYesodProc >> return ())
-- --  when (role == Hermitage.Roles.role_master) 
-- --  when (role == Hermitage.Roles.role_node)
--  
--   say role
--  
--   pid <- spawn selfNode (greet__closure "John Baptist")
--   say (show pid)
--   return ()
--   
--   let ping = do
--         say "ping!"
--         pi <- getPeers
--         say (show pi)
--  
--   forever ((liftIO $ threadDelay (10^6)) >> ping)
