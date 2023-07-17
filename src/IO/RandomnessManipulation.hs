module IO.RandomnessManipulation where

import System.Random

randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)