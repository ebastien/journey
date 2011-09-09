import Data.Ratio
import Data.List
import Data.Array.IArray
import Data.Tuple

import Data.Random.Source.PureMT (pureMT)
import Data.Random.Source.Std
import Data.Random.Distribution.Uniform (uniform)
import Data.Random.Distribution.Categorical (categorical)
import Data.Random.RVar (RVar, runRVar)
import Control.Monad.State.Lazy (evalState)
import Control.Monad (replicateM)


-- Discrete probability mass distribution
data DisM a b = DisM (a, a) [(a, b)] deriving Show

-- Cummulative probability distribution
data DisC a b = DisC (a, a) [(a, b)] deriving Show

-- Distribute a big enough number of events according to a probability mass function
-- The approximation may be wrong if the number of events is small compared to the probabiliy masses
ldistrib :: (Integral a, RealFrac b, Integral c) => DisM a b -> c -> [(a, c)]
ldistrib (DisM _ values) n = snd $ mapAccumL step (n, 0) values
    where step (m, c) (i, p) = ((m - k, c + p), (i, k))
              where k = floor (p * (fromIntegral m) / (1 - c))

-- Interpolate a discrete cummulative distribution
intercdf :: (Integral a, Fractional b) => DisC a b -> DisC a b
intercdf (DisC bounds values) = DisC bounds (concat $ interp values)
    where interp (y:[]) = [[y]]
          interp ((i, pi):y@(j, pj):xs) = rg:(interp (y:xs))
              where dp = (pj - pi) / (fromIntegral (j - i))
                    rg = [(k, pi + (fromIntegral (k - i)) * dp) | k <- [i..(j - 1)]]

-- Derive a discrete cummulative distribution
derivcdf :: (Integral a, Fractional b) => DisC a b -> DisM a b
derivcdf (DisC bounds values) = DisM bounds ((head values):(derive values))
    where derive (_:[]) = []
          derive ((i, pi):y@(j, pj):xs) = (j, (pj - pi)):(derive (y:xs))

--

partial_cdf = DisC (-330, -1) [(-330, 0), (-40, 0.2), (-20, 0.6), (-1, 1.0)] :: DisC Int Double

cdf = intercdf partial_cdf
pmf = derivcdf cdf
ldis = ldistrib pmf 10000

-- Work in progress:
-- Randomized distribution of events according to a probability mass function

pmf2cat (DisM _ values) = categorical (uncurry zip (swap . unzip $ values))

-- a categorical random variable
c = pmf2cat pmf

-- a random variable for 10K categorical draws
v = replicateM 10000 c

-- the random action
a = runRVar v StdRandom

-- the evaluation of the action from the given intial state
x = evalState a (pureMT 0)

hist :: (Ix a, Num b) => (a, a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i <- is]

-- the histogram
h = hist (-330, -1) x
