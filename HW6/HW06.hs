{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.HashMap.Strict as HS
import Data.Vector

ynToBool :: Value -> Value
ynToBool (String "Y")  = Bool True
ynToBool (String "N")  = Bool False
ynToBool (Object o)    = Object $ HS.fromList $ Prelude.map (\(l,r) -> (l,ynToBool r)) (HS.toList o)
ynToBool (Array a)     = Array (Data.Vector.map ynToBool a)
ynToBool x             = x


testYNToBool :: Bool
testYNToBool = (Bool True) == ynToBool (String "Y") && (Bool False) == ynToBool (String "N") &&
               Object (HS.singleton (T.pack "k") (String $ T.pack "Value")) == ynToBool (Object (HS.singleton (T.pack "k") (String $ T.pack "Value"))) &&
               Object (HS.singleton (T.pack "k") (String $ T.pack "T")) == ynToBool (Object (HS.singleton (T.pack "k") (String $ T.pack "Y"))) -- &&
--               Object (Data.Vector.fromList [Bool True, Bool False, String "Hello"]) == ynToBool (Object (Data.Vector.fromList [String (T.pack "Y"), String (T.pack "N"), String "Hello"]))
