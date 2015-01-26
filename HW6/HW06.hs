{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.HashMap.Strict as HS
import qualified Data.Vector as V
import Data.List
import Data.Maybe

import qualified Data.Aeson.Parser as AP
import qualified Data.Aeson.Types as AT
ynToBool :: Value -> Value
ynToBool (String "Y")  = Bool True
ynToBool (String "N")  = Bool False
ynToBool (Object o)    = Object $ HS.fromList $ Prelude.map (\(l,r) -> (l,ynToBool r)) (HS.toList o)
ynToBool (Array a)     = Array (V.map ynToBool a)
ynToBool x             = x


testYNToBool :: Bool
testYNToBool = (Bool True) == ynToBool (String "Y") && (Bool False) == ynToBool (String "N") &&
               Object (HS.singleton (T.pack "k") (String $ T.pack "Value")) == ynToBool (Object (HS.singleton (T.pack "k") (String $ T.pack "Value"))) &&
               Object (HS.singleton (T.pack "k") (String $ T.pack "T")) == ynToBool (Object (HS.singleton (T.pack "k") (String $ T.pack "Y"))) -- &&
--               Object (Data.Vector.fromList [Bool True, Bool False, String "Hello"]) == ynToBool (Object (Data.Vector.fromList [String (T.pack "Y"), String (T.pack "N"), String "Hello"]))

parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool (eitherDecode x)

data Market = Market { marketname :: T.Text, x :: Double, y :: Double, state :: T.Text }
  deriving (Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets x = fmap (\y -> case fromJSON y of (Success s) ->  s ; (Error e) -> []) (parseData x)


loadData :: IO [Market]
loadData = do
  file <- B.readFile "markets.json"
  case parseMarkets file of
    Left l  -> fail "Errore durante il parsing"
    Right r -> return r

data OrdList a = OrdList { getOrdList :: [a] }
  deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend l r = OrdList $ sort $ getOrdList l ++ getOrdList r
  mconcat l = conc l
    where conc [] = mempty
          conc (x:xs) = mappend x (conc xs)


type Searcher m = T.Text -> [Market] -> m


search :: Monoid m => (Market -> m) -> T.Text -> [Market] -> m
search f t ms = mconcat $ map f $ filter (\m -> T.isInfixOf t (marketname m)) ms

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

-- r <- loadData
-- mconcat $ map First $ map Just r

firstFound :: T.Text -> [Market] -> Maybe Market
firstFound t ms = getFirst $ search (First . Just) t ms


lastFound :: T.Text -> [Market] -> Maybe Market
lastFound t ms = getLast $ search (Last . Just) t ms
