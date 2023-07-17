module Logic.ConvolutionPadding where

import Logic.ListUtils ( applyToReversedL, lengthL, reverseL )
import Logic.RecursionSchemes ( recr )

----
-- Types of Padding
----
-- There are 3 types of padding for convolutions:
--  + Full Pad
--  + Same Pad
--  + Valid Pad
--
-- Full Pad is the simplest being a sliding
-- window in its simplest form.
--
-- If we have the following lists to convolve:
--  [1,2,3,4,5] [5,6,7]
--
-- We would convolve:
--
--       v
--      [1,2,3,4,5]
--  [7,6,5]
--       v
--       5 = 5
--
--       v
--      [1,2,3,4,5]
--    [7,6,5]
--       v
--       6+10 = 16
--
-- And so forth.
-- This is equivalent to:
--
--       v
--  [0,0,1,2,3,4,5,0,0]
--  [7,6,5]
--       v
--   0+0+5 = 7
--
--       v
--  [0,0,1,2,3,4,5,0,0]
--    [7,6,5]
--       v
--     0+6+10 = 16
--
-- Always resulting in a 3 element sum.
--
-- The result of the convolution is the following:
--  [5,16,34,52,45,28]
--
-- This is what is known as "full" padding: adding 
-- kernel length minus one zeroes to the left and
-- right.
-- 
-- Same Pad is adding padding zeroes left and right
-- at the same rate to make the resulting
-- convolution the same length as the signal.
--
-- We would convolve:
--
--       v
--    [0,1,2,3,4,5,0]
--    [7,6,5]
--       v
--       6+10 = 16
--
-- Resulting in [16,34,52,45]
--
-- Valid Pad is adding no padding. The resulting
-- convolution should only be done when the whole
-- kernel overlaps with the signal.
--
-- We would convolve:
--
--       v
--      [1,2,3,4,5]
--      [7,6,5]
--       v
--      7+12+15 = 34
--
-- Resulting in [34,52]
----

----
-- ConvolutionPadding
---
-- ConvFullPad => Full padding convolution
-- ConvValidPad => Valid padding convolution
-- ConvSamePad => Same padding convolution
----
data ConvolutionPadding a = ConvFullPad 
                        | ConvValidPad 
                        | ConvSamePad
                        | ConvSpecialPad a

----
-- fullPadLeft
----
-- Denotes taking a list of real numbers and
-- applying full padding to the left based of
-- the resulting convolution with another list.
----
-- Expanded recr denotation:
--  fullPadLeft :: Num a => [a] -> [a] -> [a]
--  fullPadLeft ys [] = ys
--  fullPadLeft ys (x:xs) = if null xs
--                              then fullPadLeft xs ys
--                              else fullPadLeft xs (0:ys)
----
fullPadLeft :: Num a => [a] -> [a] -> [a]
fullPadLeft ys = recr ys (\x xs rs -> if null xs
                                        then rs
                                        else 0:rs)
----
-- Remider!
--  recr z f [] = z
--  recr z f (x:xs) = f x xs (recr z f xs)
----

----
-- fullPadRight
----
-- Denotes taking a list of real numbers and
-- applying full padding to the right based of
-- the resulting convolution with another list.
----
-- fullPadRight is equivalent to the reversed
-- list from the result of reversing a list
-- and applying fullPadLeft
----
fullPadRight :: Num a => [a] -> [a] -> [a]
fullPadRight ys xs = applyToReversedL (`fullPadLeft` xs) ys
----

----
-- fullPad
----
-- Denotes taking a list of real numbers and
-- applying full padding based of the resulting 
-- convolution with another list.
----
fullPad :: Num a => [a] -> [a] -> [a]
fullPad ys xs = fullPadRight (fullPadLeft ys xs) xs
----

fullPadLeft2valid :: Num a => [a] -> [a] -> [a]
fullPadLeft2valid ys [] = ys
fullPadLeft2valid [] xs = []
fullPadLeft2valid (y:ys) (x:xs) = if null xs
                                    then y : fullPadLeft2valid ys xs
                                    else fullPadLeft2valid ys xs
----

fullPadRight2valid :: Num a => [a] -> [a] -> [a]
fullPadRight2valid ys xs = applyToReversedL (`fullPadLeft2valid` xs) ys

fullPad2valid :: Num a => [a] -> [a] -> [a]
fullPad2valid ys xs = fullPadRight2valid (fullPadLeft2valid ys xs) xs

fullPad2sameReversed :: Num a => [a] -> [a] -> [a]
fullPad2sameReversed [] xs = []
fullPad2sameReversed (y:ys) xs = if lengthL xs > lengthL ys
                                    then y : fullPad2same ys xs
                                    else fullPad2same ys xs

fullPad2same :: Num a => [a] -> [a] -> [a]
fullPad2same ys xs = applyToReversedL (`fullPad2sameReversed` xs) ys

fullPad2specialReversed :: (Num a, Num b, Ord b) => [a] -> b -> [a]
fullPad2specialReversed [] p     = []
fullPad2specialReversed (y:ys) p = if p > 0
                                    then y : fullPad2special (p-1) ys
                                    else fullPad2special (p-1) ys

fullPad2special :: (Num a, Num b, Ord b) => b -> [a] -> [a]
fullPad2special p xs = fullPad2specialReversed (reverseL xs) p