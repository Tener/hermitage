{-# LANGUAGE StandaloneDeriving, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, ScopedTypeVariables #-}
import Yesod
import Yesod.Json
import Database.Persist.Sqlite
import Control.Applicative
import Data.Text
import GHC.Generics
import Data.Aeson


-- Define our entities as usual

-- data ProblemStatus = PS_Fresh | PS_Running | PS_Finished deriving (Show, Read, Eq)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Problem
    description Text
    files Text
    status Text
Submission
    language String
    text String
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
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/problem/add/ ProblemAddR POST
/problem/edit/#ProblemId ProblemEditR POST
/problem/delete/#ProblemId ProblemDeleteR POST
/problem/query/#ProblemId ProblemQueryR GET
|]
-- /submission/add/ SubmissionAddR POST
-- /submission/edit/#SubmissionId SubmissionEditR POST
-- /submission/delete/#SubmissionId SubmissionDeleteR DELETE
-- /submission/query/#SubmissionId SubmissionQueryR GET
-- |]

-- Nothing special here
instance Yesod PersistTest

instance RenderMessage PersistTest FormMessage where
    renderMessage _ _ = defaultFormMessage


-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlPersist

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool


-- postProblemAddR :: Handler RepPlain
-- postProblemAddR = do
--   person <- runInputPost $ Problem
--                              <$> ireq textField "description"
--                              <*> ireq textField "files"
--                              <*> ireq textField "status"
--   pid <- runDB $ insert person
--   return $ RepPlain $ toContent $ show (pid,person)

postProblemAddR :: Handler RepJson
postProblemAddR = do
  (problem :: Problem) <- runInputPost $ Problem
                             <$> ireq textField "description"
                             <*> ireq textField "files"
                             <*> ireq textField "status"
  pid <- runDB $ insert problem
  jsonToRepJson (Data.Aeson.object [("PID" .= pid)])

postProblemEditR :: ProblemId -> Handler RepPlain
postProblemEditR pid = do
  _ <- runDB $ get404 pid
  problem <- runInputPost $ Problem
                             <$> ireq textField "description"
                             <*> ireq textField "files"
                             <*> ireq textField "status"
  runDB $ Database.Persist.Sqlite.replace pid problem
  return $ RepPlain $ toContent $ show (pid,problem)

postProblemDeleteR :: ProblemId -> Handler RepPlain
postProblemDeleteR pid = do
  _ <- runDB $ get404 pid
  runDB $ delete pid
  return $ RepPlain $ toContent $ show "Done."

getProblemQueryR :: ProblemId -> Handler RepJson
getProblemQueryR pid = do
  pr <- runDB $ get404 pid
  jsonToRepJson pr
  

-- We'll just return the show value of a problem, or a 404 if the Problem doesn't
-- exist.
-- getProblemR :: ProblemId -> Handler RepPlain
-- getProblemR problemId = do
--     problem <- runDB $ get404 problemId
--     return $ RepPlain $ toContent $ show problem

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    k <- runSqlPool (insert $ Problem "Michael" "" "fresh") pool
    print k
    warpDebug 3000 $ PersistTest pool
