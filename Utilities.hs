{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Utilities ((-:), doIf, removeJust, removeJustWithDefault, justRight, lookup2, insertMultiple, loopUntilFail, loopUntil, stopBefore, listUpdate, listUpdateFun, replaceSublist, filterJust, filterZip, cofilter, lookupList, lookupList2, mlookup, (*>), tryWithDefault, foldIterate, foldIterate2, sublist) where
import System.Environment
import Control.Monad
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import Search
import MathParser
import qualified Data.Hashable
import Data.Maybe

--combinators
(-:):: a -> (a -> b) -> b
x -: f = f x

doIf :: Bool -> (a -> a) -> (a -> a)
doIf p f = (\x -> if p then f x else x)

loopUntilFail :: (a -> Maybe a) -> a -> a
loopUntilFail f x = 
  case (f x) of 
    Just y -> loopUntilFail f y
    Nothing -> x

loopUntil :: (a -> Bool) -> (a -> a) -> a -> a    
loopUntil p f x = 
  if (p x) then x else loopUntil p f (f x)

stopBefore :: (a -> Bool) -> (a -> a) -> a -> a    
stopBefore p f x = 
  if (p (f x)) then x else stopBefore p f (f x)

--unsafe operations
removeJust:: Maybe a -> a
removeJust x = case x of Just y -> y

removeJustWithDefault :: Maybe a -> a-> a
removeJustWithDefault x y = case x of
                      Nothing -> y
                      Just z -> z

justRight :: Either a b -> b
justRight (Right x) = x

lookup2 :: (Ord a) => a -> Map.Map a b -> b
lookup2 x h = removeJust (Map.lookup x h)

lookupWithDefault:: (Ord a) => a -> Map.Map a b -> b -> b
lookupWithDefault x h y = removeJustWithDefault (Map.lookup x h) y
  
--Maps and Lists

insertMultiple :: (Ord a) => [(a, b)] -> Map.Map a b -> Map.Map a b 
insertMultiple li h = foldl (\hm -> (\(x,y) -> Map.insert x y hm)) h li


listUpdateFun :: Int -> (a-> a) -> [a] -> [a]
listUpdateFun n f li = listUpdate n (f (li !! n)) li

listUpdatesFun :: (Int -> Bool) -> (a-> a) -> [a] -> [a]
listUpdatesFun p f li = map (\(i,x) -> doIf (p i) f x) (zip [1..] li)

replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist m n li li2 =
      let 
        front = take m li
        back = drop (max m n) li
      in
        front ++ li2 ++ back

listUpdate :: Int -> a -> [a] -> [a]
listUpdate n x li = replaceSublist n (n+1) [x] li

filterJust:: [Maybe a] -> [a]
filterJust li = map removeJust (filter isJust li)

filterZip:: (b->Bool) -> [a] -> [b] -> [(a,b)]
filterZip p as bs = filter (\(x,y) -> p y) (zip as bs)

cofilter :: (b->Bool) -> [a] -> [b] -> ([a],[b])
cofilter p as bs = unzip (filterZip p as bs)

lookupList:: (Ord a) => [a] -> Map.Map a b -> [Maybe b]
lookupList as mp = fmap (flip Map.lookup mp) as

lookupList2:: (Ord a) => [a] -> Map.Map a b -> [b]
lookupList2 as mp = fmap (flip lookup2 mp) as

inj2:: a -> b -> (a,b)
inj2 x y = (x,y)

injf2:: (b->c) -> (a,b) -> (a,c)
injf2 f (x,y) = (x, f y)

inj1:: b -> a -> (a,b)
inj1 y x = (x,y)

injf1:: (a->c) -> (a,b) -> (c,b)
injf1 f (x,y) = (f x, y)

mlookup :: Int -> [a] -> Maybe a
mlookup n li = if 0<=n && n<(length li) then Just (li!!n) else Nothing 

(*>)::(a->b)->(b->c)->(a->c)
f *> g = g.f

tryWithDefault::(a->Maybe b) -> b-> a -> b
tryWithDefault f def x = 
  case (f x) of
    Nothing -> def
    Just y -> y

foldIterate:: (a->c->c)->[a]->c->c
foldIterate f as x = foldl (flip f) x as

foldIterate2:: (a->b->c->c)->[a]->[b]->c->c
foldIterate2 f as bs x = foldl (\y -> \(xa, xb) -> f xa xb y) x (zip as bs)

sublist :: Int -> Int -> [a] -> [a]
sublist m n ls =  take (n-m) . drop m $ ls
-- >=m, <n
