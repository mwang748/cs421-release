--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake num (x:xs) | num <= 0 = []
                  | otherwise = x : mytake (num - 1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop num (x:xs) | num >= 1 = mydrop (num - 1) xs
                  | otherwise = x:xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev b = rev1 b []
    where
        rev1 [] arr = arr
        rev1 (x:xs) arr = rev1 xs (x:arr)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app a1 [] = a1
app [] a2 = a2
app (x:xs) a2 = x : app xs a2

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist b = aux b []
  where 
    aux [] a = rev a
    aux (x:xs) a = aux xs $ x+1:a

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist a = aux a 0
    where
        aux [] c = c
        aux (x:xs) c = aux xs (c + x)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip a1 a2 = zipp a1 a2 []
    where 
        zipp a1 [] b = b
        zipp [] a2 b = b
        zipp (x:xs) (y:ys) b = (x,y) : zipp xs ys b

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a1 a2 = aux a1 a2 []
    where 
        aux a1 [] b = b
        aux [] a2 b = b
        aux (x:xs) (y:ys) b = x+y : aux xs ys b

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = aux 1
    where 
        aux 1 = 1:aux 1 

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = aux 0
    where
        aux a = a:aux (a+1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0:aux 0 1
    where
        aux a b = b : aux b (a+b)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add num [] = [num]
add num (x:xs) | num < x = num : x : xs
               | num == x = x:xs
               | otherwise = x: add num xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] a = a
union a [] = a
union (x:xs) (y:ys) | x == y = x: union xs ys
                    | x < y = x:union xs (y:ys)
                    | y < x = y:union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] a = []
intersect a [] = []
intersect (x:xs) (y:ys) | x == y = x: intersect xs ys
                        | x < y = intersect xs (y:ys)
                        | otherwise = intersect (x:xs) ys

--- ### powerset
myfoldr :: (a->b->b) -> b -> [a] -> b
myfoldr f z [] = z
myfoldr f z (x:xs) = f x (myfoldr f z xs)

mymap :: (a->b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


-- don't forget to put the type declaration or you will lose points!
addall x = mymap (add x)
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (addall x (powerset xs)) (powerset xs)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = mymap (+ 1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = myfoldr (+) 0
