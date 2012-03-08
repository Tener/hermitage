{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveDataTypeable #-}

module Globals where

import Data.Global
import Database.Persist.Sqlite
import Data.IORef
import Data.Text

import Data.Typeable
import Data.Conduit.Pool

deriving instance Typeable Connection
deriving instance Typeable1 Pool

globalConnectionPool :: IORef ConnectionPool
globalConnectionPool = declareIORef "hermitage::connectionPool" undefined

globalDatabasePath :: IORef Text
globalDatabasePath = declareIORef "hermitage::databasePath" "sqlite-hermitage.db"
--globalDatabasePath = declareIORef "hermitage::databasePath" ":memory:"

initConnectionPool = do
  path <- readIORef globalDatabasePath
  pool <- createSqlitePool path 10
  writeIORef globalConnectionPool pool