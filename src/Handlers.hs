{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Handlers where

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config
import qualified Data.Aeson as Aeson

import Safe

import Database.Persist
import Database.Persist.Store
import Database.Persist.GenericSql.Raw

import Datatypes
import Globals

helloWorldServe = do
  rq <- getRequest
  liftIO $ print rq
  modifyResponse $ setResponseCode 200
  writeBS "<html><body>Have a nice day...</body></html>"

jsonMime = "application/json"

serveJSON :: Snap ()
serveJSON = dir "json" $ do
  modifyResponse $ setResponseCode 200
  modifyResponse $ setContentType jsonMime
  writeLBS $ Aeson.encode (Problem "Jaja na straganie" ["jajo.txt"] :: Problem)

myServe :: Snap ()
myServe = apis <|> helloWorldServe

apis = submissionsAPI <|> problemsAPI

problemsAPI = dir "problem" (route [ ("add", method POST addProblem)
                                   , ("edit/:key", method POST editProblem)
                                   , ("delete/:key", method DELETE deleteProblem)
                                   , ("query/:key", method GET queryProblem)])

addProblem = return ()
editProblem = return ()
deleteProblem = return ()
queryProblem = return ()

submissionsAPI = dir "submission" (route [ ("add", method POST addSubmission)
                                         , ("query/:key", method GET querySubmission)
                                         , ("delete/:key", method DELETE deleteSubmission)])

addSubmission = do
  contents <- getParam "contents"
  when (contents == Nothing) finishWith412 -- TODO: report different error
  let Just contents' = contents
      Just subm = Aeson.decode' (fromSBS contents')

  sid <- liftIO (withGlobalPool $ insert (subm :: Submission))
  modifyResponse $ setResponseCode 200
  modifyResponse $ setContentType jsonMime
  writeLBS $ Aeson.encode sid

querySubmission = do
  (_, subm :: Submission) <- getValueFromParamKey

  modifyResponse $ setResponseCode 200
  modifyResponse $ setContentType jsonMime
  writeLBS $ Aeson.encode (subm :: Submission)
  liftIO $ print subm

deleteSubmission = do
  (key, (_ :: Submission)) <- getValueFromParamKey
  liftIO (withGlobalPool $ delete (key :: Key SqlPersist Submission))

  modifyResponse $ setResponseCode 200
  modifyResponse $ setContentType jsonMime
  writeLBS $ Aeson.encode ("Done." :: String)

------ UTILITY FUNCTIONS

finishWith404 :: (MonadSnap m) => m a
finishWith404 = do
  modifyResponse $ setResponseCode 404
  writeBS "404 Not found"
  r <- getResponse
  finishWith r

finishWith412 :: (MonadSnap m) => m a
finishWith412 = do
  modifyResponse $ setResponseCode 412
  writeBS "412 Invalid parameters"
  r <- getResponse
  finishWith r

-- getParamOr = getParams Just
getParamsOr :: (MonadSnap m) => Data.ByteString.ByteString -> (Data.ByteString.ByteString -> Maybe a) -> (m a) -> m a
getParamsOr param convert fail = do
  let pr s = liftIO (print s)
  value <- getParam param
  case value of
    Nothing -> pr "fail 1" >> fail
    Just x0 -> case convert x0 of
                Just x1 -> return x1
                Nothing -> pr ("fail 2",x0) >> fail

getValueFromParamKey = do
  subID <- getParamsOr "key" readMay' finishWith404
  let key = Key (PersistInt64 subID)
  subm <- liftIO (withGlobalPool $ get key)
  case subm of
    Nothing -> finishWith404
    Just x -> return (key,x)
  

fromSBS :: Data.ByteString.ByteString -> Data.ByteString.Lazy.ByteString
fromSBS sbs = Data.ByteString.Lazy.fromChunks [sbs]

fromLBS :: Data.ByteString.Lazy.ByteString -> Data.ByteString.ByteString
fromLBS lbs = Data.ByteString.concat (Data.ByteString.Lazy.toChunks lbs)

readMay' bs = readMay (Data.ByteString.Char8.unpack bs)
