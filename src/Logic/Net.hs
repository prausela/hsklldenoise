module Logic.Net where

import Logic.Convolution
import Logic.Activations
import Logic.ListUtils
import Logic.ConvolutionPadding (ConvolutionPadding(ConvSamePad, ConvValidPad, ConvSpecialPad))
import Logic.RecursionSchemes (recr)

data FeatureMap a = FMap (Activation a) [a]

instance (Show a) => Show (FeatureMap a) where
    show (FMap g ws) = "FMap " ++ show g ++ " " ++ show (lengthL ws)

onlyFilterWFM :: Num a => FeatureMap a -> [a] -> [a]
onlyFilterWFM (FMap g ws) xs = conv1D xs ws ConvSamePad

onlyInverseFilterWFM :: Num a => FeatureMap a -> [a] -> [a]
onlyInverseFilterWFM (FMap g ws) xs = conv1D xs (reverseL ws) ConvSamePad

onlyInverseFilterInputWFM :: Num a => FeatureMap a -> [a] -> [a]
onlyInverseFilterInputWFM (FMap g ws) xs = conv1D (reverseL xs) ws ConvSamePad

featureGradientFromFM :: (Floating a, Ord a) => FeatureMap a -> [a] -> [a]
featureGradientFromFM (FMap g ws) = dActivateL g

extractFeaturesWFM :: (Floating a, Ord a) => FeatureMap a -> [a] -> [a]
extractFeaturesWFM (FMap g ws) xs = activateL g (conv1D xs ws ConvSamePad)

data NetBlock a = DownBlock (FeatureMap a) (NetBlock a)
                | UpBlock   (FeatureMap a) (NetBlock a)
                | LastBlock (FeatureMap a)

instance (Show a) => Show (NetBlock a) where
    show (DownBlock fmap nb)    = "[DownBlock " ++ show fmap ++ "] " ++ show nb
    show (UpBlock fmap nb)      = "[UpBlock " ++ show fmap ++ "] " ++ show nb
    show (LastBlock fmap)       = "[LastBlock " ++ show fmap ++ "] "

denoiseNB :: (Floating a, Ord a) => [a] -> NetBlock a -> [a]
denoiseNB xs (DownBlock fmap nb) = denoiseNB (decimateL       (extractFeaturesWFM fmap xs)) nb
denoiseNB xs (UpBlock fmap nb)   = denoiseNB (linearInterpolL (extractFeaturesWFM fmap xs)) nb
denoiseNB xs (LastBlock fmap)    = extractFeaturesWFM fmap xs

----
-- prependUpBlocks
----
-- prependUpBlocks :: NetBlock a -> [FeatureMap a] -> NetBlock a
-- prependUpBlocks nb []           = nb
-- prependUpBlocks nb (fmap:fmaps) = UpBlock fmap (prependUpBlocks nb fmaps)
----
prependUpBlocks :: NetBlock a -> [FeatureMap a] -> NetBlock a
prependUpBlocks = foldr UpBlock
----

----
-- prependDownBlocks
----
-- prependDownBlocks :: NetBlock a -> [FeatureMap a] -> NetBlock a
-- prependDownBlocks nb []           = nb
-- prependDownBlocks nb (fmap:fmaps) = DownBlock fmap (prependDownBlocks nb fmaps)
----
prependDownBlocks :: NetBlock a -> [FeatureMap a] -> NetBlock a
prependDownBlocks = foldr DownBlock
----

prependSymmetricBlocks :: [FeatureMap a] -> NetBlock a -> NetBlock a
prependSymmetricBlocks fmaps nb = if isOddLengthL fmaps
                                    then error "Cannot create Symmetric BlockNet"
                                    else prependDownBlocks (prependUpBlocks nb (removeHalfL fmaps)) (leaveHalfL fmaps)

reverseSymmetricBlockNet :: [FeatureMap a] -> NetBlock a
reverseSymmetricBlockNet []           = error "Missing LastBlock FeatureMap"
reverseSymmetricBlockNet (fmap:fmaps) = prependSymmetricBlocks (reverseL fmaps) (LastBlock fmap)

symmetricBlockNet :: [FeatureMap a] -> NetBlock a
symmetricBlockNet fmaps = reverseSymmetricBlockNet (reverseL fmaps)

----
-- sameActivationFM
----
-- Denotes building a FeatureMap from a list of kernels
-- using the same activation function for every Feature
-- Map.
----
-- sameActivationFM g [] = []
-- sameActivationFM g (ws:wss) = (FMap g ws) : sameActivationFM g wss
----
sameActivationFM :: Activation a -> [[a]] -> [FeatureMap a]
sameActivationFM g = map (FMap g)

----
-- diffOutActivationFM
----
diffOutActivationFM :: Activation a -> Activation a -> [[a]] -> [FeatureMap a]
diffOutActivationFM f g [] = []
diffOutActivationFM f g (ws:wss) = if null wss
                                    then [FMap f ws]
                                    else FMap g ws : diffOutActivationFM f g wss
----

data DeltaBlock a = StepDelta [a] (DeltaBlock a)
                    | OutDelta [a]

instance (Show a) => Show (DeltaBlock a) where
    show (StepDelta d nd) = "[StepDelta " ++ show (lengthL d) ++ "] " ++ show nd
    show (OutDelta d) = "[OutDelta " ++ show (lengthL d) ++ "] "

deltaFromDB :: DeltaBlock a -> [a]
deltaFromDB (StepDelta d nd) = d
deltaFromDB (OutDelta d) = d

nextDeltaFromDB :: DeltaBlock a -> DeltaBlock a
nextDeltaFromDB (StepDelta d nd) = nd
nextDeltaFromDB (OutDelta d) = error "No more deltas"


isOutDelta :: DeltaBlock a -> Bool
isOutDelta (StepDelta d nd) = False
isOutDelta (OutDelta d)     = True

nextIsOutDelta :: DeltaBlock a -> Bool
nextIsOutDelta (StepDelta d nd) = isOutDelta nd
nextIsOutDelta (OutDelta d)     = False

sigDiff :: Num a => [a] -> [a] -> [a]
sigDiff = merge2L (-) (\_ -> error "Different signal size") (\_ -> error "Different signal size") []

sigSum :: Num a => [a] -> [a] -> [a]
sigSum = merge2L (+) (\_ -> error "Different signal size") (\_ -> error "Different signal size") []

outputErrorFromFM :: (Floating a, Ord a) => FeatureMap a -> [a] -> [a] -> [a]
outputErrorFromFM fmap xs expOut = sigDiff expOut (extractFeaturesWFM fmap xs)

deltaFromPrevDB :: (Floating a, Ord a) => [a] -> FeatureMap a -> [a] -> [a]
deltaFromPrevDB d fmap xs = mul2L (onlyInverseFilterWFM fmap d) (featureGradientFromFM fmap xs)

calculateDeltasFromNB :: (Floating a, Ord a) => [a] -> [a] -> NetBlock a -> DeltaBlock a
calculateDeltasFromNB xs expOut (DownBlock fmap nb) =
    StepDelta (deltaFromPrevDB (linearInterpolL (deltaFromDB (calculateDeltasFromNB (decimateL (extractFeaturesWFM fmap xs)) expOut nb)))   fmap xs) (calculateDeltasFromNB (decimateL (extractFeaturesWFM fmap xs)) expOut nb)
calculateDeltasFromNB xs expOut (UpBlock fmap nb) =
    StepDelta (deltaFromPrevDB (decimateL (deltaFromDB (calculateDeltasFromNB (linearInterpolL (extractFeaturesWFM fmap xs)) expOut nb)))   fmap xs) (calculateDeltasFromNB (linearInterpolL (extractFeaturesWFM fmap xs)) expOut nb)
calculateDeltasFromNB xs expOut (LastBlock fmap) =
    StepDelta                   (deltaFromPrevDB (outputErrorFromFM fmap xs expOut)                                                         fmap xs) (OutDelta (outputErrorFromFM fmap xs expOut))

applyDeltasToNB :: (Floating a, Ord a)  => a -> [a] -> DeltaBlock a -> NetBlock a -> NetBlock a
applyDeltasToNB p xs d (UpBlock fmap nb)    = UpBlock   (updateFM p (updateFMWeights fmap d xs) fmap) (applyDeltasToNB p (linearInterpolL (extractFeaturesWFM fmap xs)) (nextDeltaFromDB d) nb)
applyDeltasToNB p xs d (DownBlock fmap nb)  = DownBlock (updateFM p (updateFMWeights fmap d xs) fmap) (applyDeltasToNB p (decimateL       (extractFeaturesWFM fmap xs)) (nextDeltaFromDB d) nb)
applyDeltasToNB p xs d (LastBlock fmap)     = if isOutDelta d
                                                then LastBlock (updateFM p (updateFMWeights fmap d xs) fmap)
                                                else error "Delta blocks don't match up"

updateNB :: (Floating a, Ord a)  => a -> [a] -> DeltaBlock a -> NetBlock a -> NetBlock a
updateNB p xs d = applyDeltasToNB p xs (nextDeltaFromDB d)

updateFMWeights :: Num a => FeatureMap a -> DeltaBlock a -> [a] -> [a]
updateFMWeights (FMap g ws) d xs = conv1D (reverseL (deltaFromDB d)) xs (ConvSpecialPad (lengthL ws))

updateFM :: Num a => a -> [a] -> FeatureMap a -> FeatureMap a
updateFM p dws (FMap g ws) = FMap g (sigDiff ws (scaleL p dws))