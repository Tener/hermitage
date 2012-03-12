{-# LANGUAGE StandaloneDeriving, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

module Hermitage.DbTypes where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Conduit
import Data.Text
import Database.Persist.Postgresql
import Database.Persist.Sqlite
import GHC.Generics
import Yesod
import Yesod.Json

-- Define db entities
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Problem
    description Text
    files Text
Submission
    language String
    text String
    status Text
    problemId ProblemId
|]

deriving instance Generic Problem
instance ToJSON Problem
instance FromJSON Problem

deriving instance Generic Submission
instance ToJSON Submission
instance FromJSON Submission
