module Nuple(Nuple(..), (<++>), (!!!), (>==), module Nat, toList) where

import Nat

import Data.Kind(Type)

type Nuple :: Nat -> Type -> Type
data Nuple n x where
    Zp   :: Nuple Zero x 
    (:>) :: x -> Nuple n x -> Nuple (Succ n) x
infixr 9 :>
deriving instance Show x => Show (Nuple n x)
deriving instance Eq   x => Eq (Nuple n x)

(<++>) :: Nuple n x -> Nuple m x -> Nuple (n `NatSum` m) x
Zp      <++> n = n
(x:>xs) <++> n = x :> (xs <++> n)

(!!!) :: (n `Gt` Zero, m `Lt` n) => Nuple n x -> SNat m -> x
(x :> _)         !!! Szero   = x
(_ :> xs@(_:>_)) !!! Ssucc k = xs !!! k 
-- _                !!! _       = error "impossible"

toList :: Nuple n x -> [x]
toList Zp = []
toList (x :> xs) = x : toList xs

----------------------------------------------

instance Functor (Nuple n) where
    fmap _ Zp = Zp
    fmap f (x :> xs) = f x :> fmap f xs

---------------------------------------------
instance Applicative (Nuple Zero) where
    pure _ = Zp
    _ <*> _ = Zp

instance Applicative (Nuple n) => Applicative (Nuple (Succ n)) where
    pure n = n :> pure n
    
    f :> fs <*> x :> xs = f x :> (fs <*> xs)

------------------------------------------------

(>==) :: Nuple n x -> (x -> Nuple m y) -> Nuple (n `NatMult` m) y
Zp        >== _ = Zp
(x :> xs) >== f = f x <++> (xs >== f)

------------------------------------------------

instance Foldable (Nuple Zero) where
    foldr _ z Zp = z

instance Foldable (Nuple n) => Foldable (Nuple (Succ n)) where
    foldr f z (x :> xs) = f x (foldr f z xs)
