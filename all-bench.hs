import Control.Monad.Identity (Identity(Identity, runIdentity))
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Monoid (All(All, getAll), Ap(Ap, getAp))

allA :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f Bool
allA f = fmap getAll . getAp . foldMap (Ap . fmap All . f)

all_ :: Foldable t => (a -> Bool) -> t a -> Bool
all_ f = runIdentity . allA (Identity . f)

b :: (Integral a1, Num a2, Enum a2)
  => ((a1 -> Bool) -> [a2] -> Bool)
  -> a2
  -> Bool
b ourAll to = ourAll even [2,4..to]

_a :: Bool
_a =  b all  (100000000 :: Integer)
_a_ :: Bool
_a_ = b all_ (100000000 :: Integer)

main :: IO ()
main = defaultMain
  [ bgroup "all" $ map (f "all" all) nums
  , bgroup "all_" $ map (f "all_" all_) nums
  ]
 where
  f name ourAll n = bench (name <> " " <> show n) $ whnf (b ourAll) n
  nums :: [Integer]
  nums =
    [ 1000000
    , 10000000
    , 100000000
    ]
