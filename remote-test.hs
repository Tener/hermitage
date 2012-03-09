{-# LANGUAGE TemplateHaskell #-}
module Main where

import Remote
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

greet :: String -> ProcessM ()
greet name = say ("Hello, "++name)
badFib :: Integer -> Integer
badFib 0 = 1
badFib 1 = 1
badFib n = badFib (n-1) + badFib (n-2)
$( remotable ['greet, 'badFib] )



main :: IO ()
main = remoteInit (Just "config.txt") [Main.__remoteCallMetaData] initialProcess

initialProcess :: String -> ProcessM ()
initialProcess role = do
  say role
  selfNode <- getSelfNode
  pid <- spawn selfNode (greet__closure "John Baptist")
  say (show pid)
  return ()
  
  let ping = do
        say "ping!"
        pi <- getPeers
        say (show pi)

  forever ((liftIO $ threadDelay (10^6)) >> ping)







