module Logic.OtherUtils where

----
-- applyValueTwice
----
-- Denotes applying the same argument
-- twice to a function that receives
-- something and returns a function
-- that receives something of the same
-- nature and returns something. 
----
applyValueTwice :: (a -> a -> b) -> a -> b
applyValueTwice f x = f x x
----

----
-- fstP
----
-- Denotes the first element from a pair.
----
fstP :: (a,b) -> a
fstP (x,y) = x
----