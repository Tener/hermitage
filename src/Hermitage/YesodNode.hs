{-# LANGUAGE StandaloneDeriving, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

module Hermitage.YesodNode where

import Remote

import Yesod
import Yesod.Json
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Control.Applicative
import Data.Text
import GHC.Generics
import Data.Aeson
import Control.Concurrent
import Control.Monad
import Data.Conduit
-- Define our entities as usual
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


-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data Hermitage = Hermitage ConnectionPool

mkYesod "Hermitage" [parseRoutes|
/problem/add/ ProblemAddR POST
/problem/edit/#ProblemId ProblemEditR POST
/problem/delete/#ProblemId ProblemDeleteR POST
/problem/query/#ProblemId ProblemQueryR GET
/submission/add/ SubmissionAddR POST
/submission/edit/#SubmissionId SubmissionEditR POST
/submission/delete/#SubmissionId SubmissionDeleteR POST
/submission/query/#SubmissionId SubmissionQueryR GET
-- |]

-- Nothing special here
instance Yesod Hermitage

instance RenderMessage Hermitage FormMessage where
    renderMessage _ _ = defaultFormMessage


-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist Hermitage where
    type YesodPersistBackend Hermitage = SqlPersist

    runDB action = do
        Hermitage pool <- getYesod
        runSqlPool action pool

postProblemAddR :: Handler RepJson
postProblemAddR = do
  (problem :: Problem) <- parseJsonBody_
  pid <- runDB $ insert problem
  jsonToRepJson (Data.Aeson.object [("PID" .= pid)], problem)

postProblemEditR :: ProblemId -> Handler RepJson
postProblemEditR pid = do
  runDB $ get404 pid
  (problem :: Problem) <- parseJsonBody_
  runDB $ Database.Persist.Sqlite.replace pid problem
  jsonToRepJson (Data.Aeson.object [("PID" .= pid)], problem)

postProblemDeleteR :: ProblemId -> Handler RepJson
postProblemDeleteR pid = do
  _ <- runDB $ get404 pid
  runDB $ delete pid
  jsonToRepJson ("Done." :: Text)

getProblemQueryR :: ProblemId -> Handler RepJson
getProblemQueryR pid = do
  pr <- runDB $ get404 pid
  jsonToRepJson pr

postSubmissionAddR :: Handler RepJson
postSubmissionAddR = do
  (submission :: Submission) <- parseJsonBody_
  sid <- runDB $ insert submission
  jsonToRepJson (Data.Aeson.object [("SID" .= sid)], submission)

postSubmissionEditR :: SubmissionId -> Handler RepJson
postSubmissionEditR sid = do
  runDB $ get404 sid
  (submission :: Submission) <- parseJsonBody_
  runDB $ Database.Persist.Sqlite.replace sid submission
  jsonToRepJson (Data.Aeson.object [("SID" .= sid)], submission)

postSubmissionDeleteR :: SubmissionId -> Handler RepJson
postSubmissionDeleteR sid = do
  _ <- runDB $ get404 sid
  runDB $ delete sid
  jsonToRepJson ("Done." :: Text)

getSubmissionQueryR :: SubmissionId -> Handler RepJson
getSubmissionQueryR sid = do
  pr <- runDB $ get404 sid
  jsonToRepJson pr


openConnectionCount :: Int
openConnectionCount = 10

watchdog pool = forever $ do
  threadDelay (10^4)
  keys <- runSqlPool (do
                       subm <- selectFirst [SubmissionStatus ==. "fresh"] []
                       case subm of
                         Nothing -> return ()
                         Just ent -> do
                                   update (entityKey ent) [SubmissionStatus =. "processing"]
                       return subm
                     ) pool
  when (keys /= Nothing) (print keys)

runPGSQL what = withPostgresqlPool "host=localhost port=5432 dbname=hermitage user=hermitage password=hermitage123" openConnectionCount what
runSQLITE what = withSqlitePool "test.db3" openConnectionCount what

runYesod :: IO ()
runYesod = runPGSQL $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    forkIO (watchdog pool)
    k <- runSqlPool (insert $ Problem "Michał" "") pool
    print k
    print =<< runSqlPool (insert $ Submission "haskell" "main = return ()" "fresh" k) pool
    warpDebug 3000 $ Hermitage pool

runYesodProc :: ProcessM ()
runYesodProc = liftIO runYesod

$( remotable ['runYesod, 'runYesodProc] )