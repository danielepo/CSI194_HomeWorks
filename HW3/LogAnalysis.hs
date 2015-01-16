{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 

import Data.List
import Data.Char
import Log

parseInt :: String -> MaybeInt
parseInt = parseIntW 0
    where
        parseIntW ac []   = ValidInt ac
        parseIntW ac (x:xs)
          |  isNumber x   = parseIntW ((10 * ac ) + digitToInt x) xs
          |  otherwise    = InvalidInt

pmw :: [String] -> String -> MaybeLogMessage
pmw ("W":i:ws) m = case parseInt2 i of
        ValidInt v -> ValidLM (LogMessage Warning v (unwords ws))
        InvalidInt -> InvalidLM m
pmw ("I":i:ws) m = case parseInt2 i of
        ValidInt v -> ValidLM (LogMessage Info v (unwords ws))
        InvalidInt -> InvalidLM m
pmw ("E":e:i:ws) m = case parseInt2 e of
        ValidInt v -> (case parseInt2 i of 
                      ValidInt j   -> ValidLM (LogMessage (Error v) j (unwords ws))
                      InvalidInt   -> InvalidLM m)
        InvalidInt -> InvalidLM m
pmw (_) m      = InvalidLM m

parseMessage :: String -> MaybeLogMessage
parseMessage m = pmw (words m) m

parseInt2 :: String -> MaybeInt
parseInt2 s = case reads s :: [(Int, String)] of
    [(n, [])] -> ValidInt n
    _ -> InvalidInt

parseInt3 :: String -> Int
parseInt3 = foldl (\x y -> x * 10 + (digitToInt y)) 0

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly []     = []
validMessagesOnly (x:xs) = case x of
                           ValidLM m   -> m : validMessagesOnly xs
                           InvalidLM _ -> validMessagesOnly xs

						   
parse :: String -> [LogMessage]
parse x = validMessagesOnly (pw (lines x))

pw :: [String] -> [MaybeLogMessage]
pw []     = []
pw (x:xs) = (parseMessage x):(pw xs)

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t1 _) (LogMessage _ t2 _) 
        | t1 < t2   = LT
        | t1 > t2   = GT
        | otherwise = EQ
		
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages l1 = sortBy compareMsgs l1 	

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = wwww (sortMessages x)

wwww :: [LogMessage] -> [String]
wwww [] = []
wwww (x:xs) = if gt50 x then unmount x : wwww xs else wwww xs

gt50 :: LogMessage -> Bool
gt50 (LogMessage (Error i) _ _) = i >= 50
gt50 (LogMessage _ _ _) = False

unmount :: LogMessage -> String
unmount (LogMessage _ _ s) = s

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s ls = filter (\x -> containsCondiment s x) ls

containsCondiment :: String -> LogMessage -> Bool
containsCondiment c m = ccw (messageString m) c c

ccw :: String -> String -> String -> Bool
ccw _ []  _ = True
ccw [] _ _  = False
ccw (l:ls) (m:ms) n
   | (toLower l) == (toLower m) = ccw ls ms n
   | otherwise                  = ccw ls n n

messageString:: LogMessage -> String
messageString (LogMessage _ _ s) =s

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s x = wwww2 s (sortMessages x)

-- strange :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
-- strange f g x = (f x) || (g x)

-- wwww2 :: String -> [LogMessage] -> [String]
-- wwww2 _ [] = []
-- wwww2 s (x:xs) = if strange gt50 (containsCondiment s) x then unmount x : wwww2 s xs else wwww2 s xs


(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = (f x) || (g x)

wwww2 :: String -> [LogMessage] -> [String]
wwww2 _ [] = []
wwww2 s (x:xs) = if (gt50 ||| (containsCondiment s)) x then unmount x : wwww2 s xs else wwww2 s xs


