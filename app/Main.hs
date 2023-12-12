module Main (main) where

import Nuple
import Op

_opM :: Op' (S (S Z)) (S Z) Int Int
_opM = Op $ \(a :> b :> Zp) -> (a * b) :> Zp

_opN :: Op' (S Z) (S (S Z)) Int Int
_opN = Op $ \(a :> Zp) -> (a * 2) :> (a * 3) :> Zp

test :: Nuple (S (S (S Z))) Int
test = 1 :> 2 :> 3 :> Zp

multAll :: Nuple (S (S Z)) Int -> Nuple (S (S Z)) Int
multAll x = (_opM >>- _opN) >>-> x

main :: IO ()
main = do
    print $ test !!! Ss (Ss Sz)
    print $ test !!! Ss Sz
    print $ test !!! Sz

    print $ nFilter even (test <++> (4 :> Zp))
    print $ multAll $ nTake (Ss (Ss Sz)) test