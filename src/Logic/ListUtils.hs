module Logic.ListUtils where

import Logic.RecursionSchemes ( recr )
import Logic.OtherUtils ( fstP )

----
-- lengthL
----
-- Denotes the length of a list.
----
lengthL :: [a] -> Integer
lengthL = foldr (\x -> (+) 1) 0
----
-- Reminder!
--  foldr f z (x:xs) = f x (foldr f z xs)
----

isOddLengthL :: [a] -> Bool
isOddLengthL []     = False
isOddLengthL (x:xs) = isEvenLengthL xs

isEvenLengthL :: [a] -> Bool
isEvenLengthL []     = True
isEvenLengthL (x:xs) = isOddLengthL xs

----
-- scaleL
----
-- Denotes multiplying every element
-- in a list by a factor.
----
-- scaleL :: Num a => a -> [a] -> [a]
-- scaleL n [] = []
-- scaleL n (x:xs) = x*n : scaleL n xs
----
scaleL :: Num a => a -> [a] -> [a]
scaleL n = map (* n)
----

----
-- headL
----
-- headL ::[a] -> a
-- headL [] = error "Cannot perform on empty list"
-- headL (x:xs) = x
----
headL :: [a] -> a
headL = foldr const (error "Cannot perform on empty list")
----
-- Reminder!
--  foldr f z (x:xs) = f x (foldr f z xs)
----

----
-- tailL
----
-- Denotes a list without its first element.
----
-- tailL ::[a] -> [a]
-- tailL [] = error "Cannot perform on empty list"
-- tailL (x:xs) = xs
----
tailL :: [a] -> [a]
tailL = recr (error "Cannot perform on empty list") (\_ xs _ -> xs)
----
-- Reminder!
--  recr z f [] = z
--  recr z f (x:xs) = f x xs (recr z f xs)
----

----
-- reverseL
----
-- Denotes reversing the order of the elements in 
-- a list.
----
-- Easier to understand denotation
--  reverseL ::[a] -> [a]
--  reverseL [] = []
--  reverseL (x:xs) = reverseL xs ++ [x]
----
-- Pre-fold denotation
--  reverseL ::[a] -> [a]
--  reverseL [] = []
--  reverseL (x:xs) = (\x xs -> xs ++ [x]) x (reverseL xs)
----
reverseL :: [a] -> [a]
reverseL = foldr (\x xs -> xs ++ [x]) []
----
-- Reminder!
--  foldr f z (x:xs) = f x (foldr f z xs)
----

----
-- repeatL
----
repeatL :: Integer -> a -> [a]
repeatL 0 x  = []
repeatL n x = x : repeatL (n-1) x
----

----
-- pairL
----
-- Denotes pairing every even-positioned
-- element with the previous odd-positioned
-- element and every odd-positioned element
-- with the following element from a
-- list. If list is odd-lengthed, the last
-- element is discarded.
--
-- For example:
--
--      [1,2,3,4]   -> [(1,2),(3,4)]
--      [1,2,3,4,5] -> [(1,2),(3,4)]
--
----
pairL :: [a] -> [(a,a)]
pairL [] = []
pairL [x] = []
pairL (x:y:xs) = (x,y) : pairL xs
----

----
-- applyPairwiseL
----
-- Denotes turning a list into a list of 
-- pairs and apply some transformation
-- to the pairs turning them into a
-- single element.
----
applyPairwiseL :: ((a,a) -> b) -> [a] -> [b]
applyPairwiseL f xs = map f (pairL xs)
----

----
-- merge2L
----
merge2L :: (a -> a -> a) -> ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a] -> [a] -> [a]
merge2L f g h z (x:xs) (y:ys) = f x y : merge2L f g h z xs ys
merge2L f g h z [] [] = z
merge2L f g h z xs [] = g xs
merge2L f g h z [] ys = h ys
----

----
-- sum2L
----
-- Denotes the sum of a list of real numbers with
-- a list of real numbers. This is interpreted as
-- every element of the first list is summed with
-- the element at the same position in the second
-- list.
-- Should a list have more elements than the 
-- other, the elements at the positions which the 
-- other list does not posses shall be 
-- interpreted as zero.
----
-- sum2L :: Num a => [a] -> [a] -> [a]
-- sum2L (x:xs) (y:ys) = x+y : sum2L xs ys
-- sum2L xs [] = xs
-- sum2L [] ys = ys
----
sum2L :: Num a => [a] -> [a] -> [a]
sum2L = merge2L (+) id id []
----

----
-- diff2L
----
-- Denotes the difference between a list of real 
-- numbers with a list of real numbers. This is 
-- interpreted as every element of the first list 
-- is subtracted the element at the same position 
-- in the second list.
-- Should the first list have more elements than 
-- the other, the elements at the positions which 
-- the other list does not posses shall be 
-- interpreted as zero.
-- Should the second list be shorter, the
-- elements at the positions which the other
-- list does not posses shall be ignored
-- resulting in a list of the same size as the
-- fist list.
----
-- diff2L :: Num a => [a] -> [a] -> [a]
-- diff2L (x:xs) (y:ys) = x-y : diff2L xs ys
-- diff2L xs [] = xs
-- diff2L [] ys = []
----
diff2L :: Num a => [a] -> [a] -> [a]
diff2L = merge2L (-) id (const []) []
----

----
-- mul2L
----
-- Denotes the element-wise multiplication
-- between two lists.
----
mul2L :: Num a => [a] -> [a] -> [a]
mul2L = merge2L (*) (\_ -> error "Lists must be of same length") (\_ -> error "Lists must be of same length") []
----

----
-- applyToReversedL
----
applyToReversedL :: ([a] -> [a]) -> [a] -> [a]
applyToReversedL f = reverseL . f . reverseL
----

----
-- removeEvenL
----
-- Denotes removing every even-positioned
-- element from a list (starts at 0).
----
removeEvenL :: [a] -> [a]
removeEvenL [] = []
removeEvenL (x:xs) = leaveEvenL xs
----

----
-- leaveEvenL
----
-- Denotes removing every non-even-positioned
-- element from a list (starts at 0).
----
leaveEvenL :: [a] -> [a]
leaveEvenL [] = []
leaveEvenL (x:xs) = x : removeEvenL xs
----

----
-- decimateL
----
-- Denotes removing every non-even
-- positioned element from a list 
-- that starts at position 0.
-- 
-- For example:
--
--      [1,2,3,4,5]   -> [1,3,5]
--
--      [1,2,3,4,5,6] -> [1,3,5]
--
----
decimateL :: [a] -> [a]
decimateL = leaveEvenL
----

----
-- removeHalfL
----
-- Denotes removing the first half of elements
-- from a list.
----
removeHalfL :: [a] -> [a]
removeHalfL [] = []
removeHalfL (x:xs) = applyToReversedL leaveHalfL xs
----

----
-- leaveHalfL
----
-- Denotes removing everything except the first
-- half of elements from a list
----
leaveHalfL :: [a] -> [a]
leaveHalfL [] = []
leaveHalfL (x:xs) = x : applyToReversedL removeHalfL xs
----

----
-- linearInterpolL
----
-- linearInterpolL :: Fractional a => [a] -> [a]
-- linearInterpolL [] = []
-- linearInterpolL (x:xs) = if null xs
--                             then x : x : linearInterpolL xs
--                             else x : (x + headL xs) / 2 : linearInterpolL xs
----
linearInterpolL :: Fractional a => [a] -> [a]
linearInterpolL = recr [] (\x xs rs -> if null xs
                                        then x : x : rs
                                        else x : (x + headL xs) / 2 : rs)
----

----
-- chooseBetween2L
----
chooseBetween2L :: ([a] -> [a] -> Bool) -> [a] -> [a] -> [a]
chooseBetween2L f xs ys = if f xs ys
                            then xs
                            else ys
----

----
-- shortestOf2L
----
-- shortestOf2L :: [a] -> [a] -> [a]
-- shortestOf2L xs ys = if lengthL xs < lengthL ys
--                         then xs
--                         else ys
----
shortestOf2L :: [a] -> [a] -> [a]
shortestOf2L = chooseBetween2L (\xs ys -> lengthL xs < lengthL ys)
----

----
-- largestOf2L
----
-- largestOf2L :: [a] -> [a] -> [a]
-- largestOf2L xs ys = if lengthL xs >= lengthL ys
--                         then xs
--                         else ys
----
largestOf2L :: [a] -> [a] -> [a]
largestOf2L = chooseBetween2L (\xs ys -> lengthL xs >= lengthL ys)
----

----
-- chooseL
----
-- chooseL ::([a] -> [a] -> [a]) -> [[a]] -> [a]
-- chooseL f [] = error ""
-- chooseL f (xs:xss) = if null xss
--                          then xs
--                          else xs `f` chooseL xss
----
chooseL :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
chooseL f = recr
                (error "Cannot choose from empty list")
                (\xs xss rs -> if null xss
                                then xs
                                else xs `f` rs)
----

----
-- shortestL
----
-- shortestL :: [[a]] -> [a]
-- shortestL [] = error ""
-- shortestL (xs:xss) = if null xss
--                         then xs
--                         else xs `shortestOf2L` shortestL xss
----
shortestL :: [[a]] -> [a]
shortestL = chooseL shortestOf2L
----

----
-- largestL
----
-- largestL ::[[a]] -> [a]
-- largestL [] = error ""
-- largestL (xs:xss) = if null xss
--                         then xs
--                         else xs `largestOf2L` largestL xss
----
largestL :: [[a]] -> [a]
largestL = chooseL largestOf2L
----