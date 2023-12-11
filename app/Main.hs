module Main (main) where

import Nuple
import Op

_opM :: Op' (Succ (Succ Zero)) (Succ Zero) Int Int
_opM = Op $ \(a :> b :> Zp) -> (a * b) :> Zp

_opN :: Op' (Succ Zero) (Succ (Succ Zero)) Int Int
_opN = Op $ \(a :> Zp) -> (a * 2) :> (a * 3) :> Zp

test :: Nuple ('Succ ('Succ ('Succ 'Zero))) Integer
test = 1 :> 2 :> 3 :> Zp

main :: IO ()
main = do
    print $ test !!! Ssucc (Ssucc Szero)
    print $ test !!! Ssucc Szero
    print $ test >== (\x -> x :> x :> Zp)

