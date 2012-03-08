{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Datatypes where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Conduit.Pool (withResource)
import Data.IORef
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

import Globals

share [mkPersist sqlSettings, mkSave "entityDefs"] [persist|
Problem
    description String
    files [String]
Submission
    language String
    text String
    problemId ProblemId
|]

migrateAllDatatypes = withGlobalPool $ do
               runMigration $ migrate entityDefs (undefined :: Problem)
               runMigration $ migrate entityDefs (undefined :: Submission)

withGlobalPool fun = do
  pool <- readIORef globalConnectionPool
  withResource pool $ runSqlConn fun

deriving instance Generic Problem
instance ToJSON Problem
instance FromJSON Problem

deriving instance Generic Submission
instance ToJSON Submission
instance FromJSON Submission

insertTestData = withGlobalPool $ do
     keys <- mapM insert [ Problem ("Problem #" ++ show i) [] | i <- [1..10]]
     mapM_ insert [ Submission "haskell" ("main = print " ++ show i) k | (k,i) <- zip keys [1..]]

