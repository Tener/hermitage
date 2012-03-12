{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Hermitage.Types where

import Remote
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)

data HermitageMessages = Shutdown deriving (Eq,Show,Ord,Read,Generic,Typeable)

instance Binary HermitageMessages where
    put = genericPut
    get = genericGet
