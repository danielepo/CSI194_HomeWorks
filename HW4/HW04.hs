{-
Name: Daniele
Notes:

-}


module HW04 where

import BST
import Data.Maybe
import Data.Char
{-
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ elm Leaf = Node (Leaf) elm (Leaf)
insertBST f elm (Node l n r)
    | (f elm n) == GT = Node l n (insertBST f elm r)
    | otherwise       = Node (insertBST f elm l) n r
-}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

allCaps :: [String] -> Bool
allCaps xs = foldl (\x y -> if isJust (safeHead y) then isUpper (fromJust (safeHead y)) && x else False) (not (null xs)) xs
 --foldl (\x y -> if isJust (safeHead y) then (isJust (safeHead y)) && x else False) True xs
