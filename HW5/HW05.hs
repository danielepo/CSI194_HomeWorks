{- daniele -}

module HW05 where

import Ring
import Parser
import Data.List
import Data.Maybe
import Data.Bool

data Mod5 = MkMod Integer
  deriving (Show, Eq)

instance Ring Mod5 where
  addId  = MkMod 0
  addInv (MkMod x) = MkMod (negate x)
  mulId  = MkMod 1

  add (MkMod x) (MkMod y) = MkMod ((x + y) `mod` 5)
  mul (MkMod x) (MkMod y) = MkMod ((x * y) `mod` 5)


testMod5Ring :: Bool
testMod5Ring = (MkMod 0 == addId `add` addId) && (MkMod 1 == addId `add` MkMod 1) &&
               (MkMod 4 == MkMod 1 `add` MkMod 3) && (MkMod 1 == MkMod 4 `add` MkMod 2) &&
               (MkMod 4 == MkMod 2 `mul` MkMod 2) && (MkMod 2 == MkMod 2 `mul` mulId) &&
               (MkMod (-1) == addInv (MkMod 1)) && (parse "MkMod 3" == Just (MkMod 3, ""))

instance Parsable Mod5 where
  parse str = case stripPrefix "MkMod " str of
    Just rest -> case reads rest :: [(Integer,String)] of
      ((n,remaining):xs) -> Just ((MkMod n),remaining)
      _                  -> Nothing
    _         -> Nothing


data Mat2x2 = Mat ((Integer,Integer),(Integer,Integer))
  deriving (Show, Eq)


instance Ring Mat2x2 where
  addId = Mat ((0,0),(0,0))
  mulId = Mat ((1,0),(0,1))
  addInv (Mat ((x1,x2),(y1,y2))) = Mat ((negate x1,negate x2),(negate y1,negate y2))

  add (Mat ((x1,x2),(y1,y2))) (Mat ((a1,a2),(b1,b2))) = Mat ((x1+a1,x2+a2),(y1+b1,y2+b2))
  mul (Mat ((x1,x2),(y1,y2))) (Mat ((a1,a2),(b1,b2))) = Mat ((x1 * a1 + x2 * b1 , x1 * a2 + x2 * b2),(y1 * a1 + y2 * b1 , y1 * a2 + y2 * b2))

instance Parsable Mat2x2 where
  parse str = case stripPrefix "[" str of
    Just rem1 -> case listToMaybe (reads rem1 :: [([Integer],String)]) of
      Just (l11:l12:l1s,rem2) -> case listToMaybe (reads rem2 :: [([Integer],String)]) of
        Just (l21:l22:l2s, rem3) -> case stripPrefix "]" rem3 of
           Just remaining -> Just (Mat ((l11,l12),(l21,l22)),remaining)
           Nothing -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing

testMat2x2Ring :: Bool
testMat2x2Ring = addId ==  Mat ((0,0),(0,0)) &&
                 Mat ((1,1),(1,1)) == mulId `mul` Mat ((1,1),(1,1)) &&
                 Mat ((1,2),(3,4)) == Mat ((1,1),(1,1)) `add` Mat ((0,1),(2,3)) &&
--                 parse "[1,2][3,4]" == Nothing &&
                 parse "[[1,2][3,4]]" == Just (Mat ((1,2),(3,4)), "")

instance Ring Bool where
  addId = False
  mulId = True
  addInv = not

  add x y = (x && (not y)) || ((not x) && y)
  mul x y = x && y

instance Parsable Bool where
  parse str
    | Just rest <- stripPrefix "True" str  = Just (True, "")
    | Just rest <- stripPrefix "False" str = Just (False, "")
    | otherwise                            = Nothing

testBoolRing :: Bool
testBoolRing = addId == False && False == (add True True) && True == (add True False) &&
               True == (mul True True) && False == (mul True False) && True == (mul True mulId) && False == (mul mulId False)
               && parse "True" == Just (True, "")
