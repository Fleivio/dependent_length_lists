{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Nuple
import Op

opM :: Op (S (S Z)) (S Z) Int Int
opM = Op $ \(a :> b :> Zp) -> (a * b) :> Zp

opN :: Op (S Z) (S (S Z)) Int Int
opN = Op $ \(a :> Zp) -> (a * 2) :> (a * 3) :> Zp

test :: Nuple (S (S (S Z))) Int
test = 1 :> 2 :> 3 :> Zp

combinedOp :: Op ('S ('S 'Z)) ('S ('S 'Z)) Int Int
combinedOp = 
    Op.do
    opM
    opN

main :: IO ()
main = do
    print $ nFilter even (test <++> 4 :> Zp)
    print $ combinedOp `app` (1 :> 2 :> Zp)