module Logic.Convolution where

import Logic.ListUtils ( sum2L, largestOf2L, shortestOf2L )
import Logic.ConvolutionPadding (ConvolutionPadding (ConvFullPad, ConvSamePad, ConvValidPad, ConvSpecialPad), fullPad2same, fullPad2valid, fullPadLeft2valid, fullPadRight2valid, fullPad2special)

----
-- elemConv1D
----
-- Denotes the convolution of a list with a single
-- real number
----
-- Easier to understand denotation:
-- elemConv1D :: Num a => [a] -> a -> [a]
-- elemConv1D [] y = []
-- elemConv1D (x:xs) y = x*y : elemConv1D xs y
----
elemConv1D:: Num a => [a] -> a -> [a]
elemConv1D xs y = map (*y) xs
----
-- Reminder!
--  map f [] = []
--  map f (x:xs) = f x : map xs
----

----
-- nElemConv1D
----
-- Denotes the convolutions of a list of real numbers with
-- a list of real numbers
----
-- Easier to understand denotation:
--  nElemConvOld1 :: Num a => [a] -> [a] -> [[a]]
--  nElemConvOld1 [] ys = []
--  nElemConvOld1 (x:xs) ys = elemConv ys x : nElemConvOld1 xs ys
----
nElemConv1D:: Num a => [a] -> [a] -> [[a]]
nElemConv1D xs ys = map (elemConv1D ys) xs 
----
-- Reminder!
--  map f [] = []
--  map f (x:xs) = f x : map xs
----

----
-- sum2ElemConv1D
----
-- Denotes the addition of a list of real numbers with
-- a list of real numbers, shifting one element to the
-- right the second list
----
-- Easier to understand denotation:
--  sum2ElemConvOld1 :: Num a => [a] -> [a] -> [a]
--  sum2ElemConvOld1 [] ys = ys
--  sum2ElemConvOld1 (x:xs) ys = x : sumL xs ys
----
sum2ElemConv1D :: Num a => [a] -> [a] -> [a]
sum2ElemConv1D [] ys = ys
sum2ElemConv1D (x:xs) ys = x : sum2L xs ys
----

----
-- sumNElemConv1D
----
-- Denotes the addition of every list of real number
-- inside a list of lists of real number, shifting, for
-- every sum of lists performed, the next one the position
-- it occupies on the list minus one
----
-- Easier to understand denotation:
--  sumNElemConvOld1 :: Num a => [[a]] -> [a]
--  sumNElemConvOld1 [] = []
--  sumNElemConvOld1 (xs:xss) = sum2ElemConv xs (sumNElemConvOld1 xss)
----
sumNElemConv1D :: Num a => [[a]] -> [a]
sumNElemConv1D = foldr sum2ElemConv1D []
----
-- Easier denotation to understand application:
--  sumNElemConv xs = foldr sum2ElemConv [] xs
----
-- Reminder!
--  foldr f z (x:xs) = f x (foldr f z xs)
----

----
-- fullPadConv
----
-- Denotes the convolution of a list of real numbers
-- with another list of real numbers using full
-- padding (refer to ConvolutionPadding module).
----
fullPadConv1D :: Num a => [a] -> [a] -> [a]
fullPadConv1D xs ys = sumNElemConv1D (nElemConv1D xs ys)
----

----
-- conv1D
----
-- Denotes the convolution of a list of real numbers
-- with another list of real numbers with a specified
-- padding (full, same, valid or special).
----
conv1D :: (Num a, Num b, Ord b) => [a] -> [a] -> ConvolutionPadding b -> [a]
conv1D xs ys ConvFullPad        = fullPadConv1D xs ys
conv1D xs ys ConvSamePad        = fullPad2same (fullPadConv1D xs ys) xs
conv1D xs ys ConvValidPad       = fullPad2valid (fullPadConv1D xs ys) (shortestOf2L xs ys)
conv1D xs ys (ConvSpecialPad p) = fullPad2special p (fullPadConv1D xs ys)
----