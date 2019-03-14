module MachineLearning where

type SampleSet = [(Double, Double)]

gradientDescent ::  [(Double, Double)] -> Double -> (Double, Double)
gradientDescent sampleSet alpha = go (0, 0)
  where
    m = fromIntegral (length sampleSet)
    h delta0 delta1 x = delta0 + delta1*x
    derivative0 delta0 delta1 = (1.0 / m) * alpha * foldr (\(x,y) acc -> acc + (h delta0 delta1 x) - y) 0.0 sampleSet
    derivative1 delta0 delta1 = (1.0 / m) * alpha * foldr (\(x,y) acc -> acc + (h delta0 delta1 x - y)*x) 0.0 sampleSet
    go (delta0, delta1) =
      let d0 = derivative0 delta0 delta1
          d1 = derivative1 delta0 delta1
      in if abs d0 < 1e-10 && abs d1 < 1e-10 then (delta0, delta1) else go (delta0 - d0, delta1 - d1)
      -- in (d0, d1)

main = gradientDescent (fmap (\ x -> (x, x*1)) [0..100]) 0.0003
