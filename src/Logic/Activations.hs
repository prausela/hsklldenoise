module Logic.Activations where

data Activation a = Tanh a | LeakyReLU a | ReLU

instance (Show a) => Show (Activation a) where
    show (Tanh beta)    = "Tanh " ++ show beta
    show (LeakyReLU t)  = "LeakyReLU " ++ show t
    show ReLU           = "ReLU"

----
-- Given an activation and a list, returns the activation
-- applied to the list.
----
activateL :: (Floating a, Ord a) => Activation a -> [a] -> [a]
activateL g = map (activationF g)
----

----
-- Given an activation and a floating value
-- applies the activation function to the value.
----
activationF :: (Floating a, Ord a) => Activation a -> a -> a
activationF (Tanh beta)      = tanActivation beta
activationF ReLU             = reLUActivation
activationF (LeakyReLU t)    = leakyReLUActivation t
----

----
-- Given an activation and a list, returns the derivative of the
-- activation applied to the list.
----
dActivateL :: (Floating a, Ord a) => Activation a -> [a] -> [a]
dActivateL g = map (dActivationF g)
----

----
-- Given an activation and a floating value
-- applies the derivative of the activation function 
-- to the value.
----
dActivationF :: (Floating a, Ord a) => Activation a -> a -> a
dActivationF (Tanh beta)     = dtanActivation beta
dActivationF ReLU            = dreLUActivation
dActivationF (LeakyReLU t)   = dleakyReLUActivation t
----

----
-- tanActivation
----
tanActivation :: Floating a => a -> a -> a
tanActivation b x = tanh (b*x)
----

----
-- dtanActivation
----
-- Denotes the derivative of tanActivation.
----
dtanActivation :: Floating a => a -> a -> a
dtanActivation b x = 1 - tanh (b*x) * tanh (b*x)
----

----
-- reLUActivation
----
-- reLUActivation::Num a => a
-- reLUActivation x = if x > 0
--                     then x
--                     else 0
----
reLUActivation :: (Ord a, Num a) => a -> a
reLUActivation = leakyReLUActivation 0
----

----
-- dreLUActivation
----
-- Denotes the derivative of reLUActivation.
----
dreLUActivation :: (Ord a, Num a) => a -> a
dreLUActivation = dleakyReLUActivation 0
----

----
-- leakyReLUActivation
----
leakyReLUActivation :: (Ord a, Num a) => a -> a -> a
leakyReLUActivation t x = if x > 0
                            then x
                            else t*x
----

----
-- dleakyReLUActivation
----
-- Denotes the derivative of leakyReLUActivation.
----
dleakyReLUActivation :: (Ord a, Num a) => a -> a -> a
dleakyReLUActivation t x = if x > 0
                            then 1
                            else t
----