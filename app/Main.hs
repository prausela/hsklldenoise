module Main (main) where

import IO.RandomnessManipulation
import Logic.Net
import Logic.ListUtils (repeatL, lengthL)
import Logic.Activations (Activation(ReLU, Tanh))
import Logic.Net (denoiseNB, calculateDeltasFromNB)
import Activations (Activation(LeakyReLU))

main :: IO ()
main = do
        print $ updateNB 0.00001 (repeatL 1024 0.1)
                (calculateDeltasFromNB (repeatL 1024 0.1) (repeatL 1024 0.1) (symmetricBlockNet (diffOutActivationFM (Tanh 1.0) (LeakyReLU 0.2) (repeatL 9 (repeatL 64 0.5)))))
                (symmetricBlockNet (diffOutActivationFM (Tanh 1.0) ReLU (repeatL 9 (repeatL 64 0.5))))