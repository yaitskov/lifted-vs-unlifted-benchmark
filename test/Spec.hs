{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures, TypeApplications #-}

import Criterion.Main
import UnliftedBoxed
import System.Random as SR
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.List (foldl')

main :: IO ()
main = do
  g <- SR.initStdGen
  let (p1, g') = uniform g
      (p2, _) = uniform g'
  liftedBools :: [Bool] <- evaluate (force (fmap snd $! zip [0..10000] (cycle [p1, p2])))
  let unliftedBools :: MyUnList MyUnliftedBoxed = fromLiftedList liftedBools fromBool'


  defaultMain [
    bgroup "Fold Lifted" [
      bench "Left" $ whnf (foldl @[] (&&) True) liftedBools
    ],
    bgroup "Fold Lifted Prime" [
      bench "Left" $ whnf (foldl' @[] (&&) True) liftedBools
    ],
    bgroup "Fold Unlifted" [
      bench "Left" $ whnf (\_x -> toBool' (foldlUn sAnd STrue unliftedBools))  ()
    ]
    ]
