{-# LANGUAGE DeriveDataTypeable #-}

module Hermitage.MessageTypes where

import Remote
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)

data HermitageMessages = Shutdown deriving (Eq,Show,Ord,Read,Data,Typeable)

instance Binary HermitageMessages where
    put = genericPut
    get = genericGet
