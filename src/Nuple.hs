{-#LANGUAGE PolyKinds #-}
module Nuple(
    Nuple(..), CNuple(..), ENuple(..),
    (<++>), (!!!), (>==),
    toList, fromList,
    nTake, nDrop, nHead, nTail, nInit, nLast,
    nFilter,
    module Nat) where

import Nat

import Data.Kind(Type, Constraint)

type Nuple :: Nat -> Type -> Type
data Nuple n x where
    Zp   :: Nuple Z x 
    (:>) :: x -> Nuple n x -> Nuple (S n) x
infixr 9 :>
deriving instance Eq   x => Eq (Nuple n x)

instance Show x => Show (Nuple n x) where
    show xs = "#" ++ (show $ toList xs)

(<++>) :: Nuple n x -> Nuple m x -> Nuple (n `NatSum` m) x
Zp      <++> n = n
(x:>xs) <++> n = x :> (xs <++> n)

(!!!) :: (Lt m n, Gt n Z) => Nuple n x -> SNat m -> x
(x :> _)         !!! Sz   = x
(_ :> xs@(_:>_)) !!! Ss k = xs !!! k
-- This cases are impossible, but GHC doesn't know that
_                !!! _    = error "impossible case" 

nTake :: SNat n -> Nuple m x -> Nuple (n `NatMin` m) x
nTake Sz     _         = Zp
nTake (Ss _) Zp        = Zp
nTake (Ss n) (x :> xs) = x :> nTake n xs

nDrop :: SNat n -> Nuple m x -> Nuple (m `NatSub` n) x
nDrop Sz     xs        = xs
nDrop (Ss _) Zp        = Zp
nDrop (Ss n) (_ :> xs) = nDrop n xs 

nHead :: Nuple (S n) x -> x
nHead (x :> _) = x

nTail :: Nuple (S n) x -> Nuple n x
nTail (_ :> xs) = xs

nInit :: Nuple (S n) x -> Nuple n x
nInit (_ :> Zp) = Zp
nInit (x :> xs@(_:>_)) = x :> nInit xs

nLast :: Nuple (S n) x -> x
nLast (x :> Zp)        = x
nLast (_ :> xs@(_:>_)) = nLast xs

toList :: Nuple n x -> [x]
toList Zp = []
toList (x :> xs) = x : toList xs

----------------------------------------------

instance Functor (Nuple n) where
    fmap _ Zp = Zp
    fmap f (x :> xs) = f x :> fmap f xs

---------------------------------------------
instance Applicative (Nuple Z) where
    pure _ = Zp
    _ <*> _ = Zp

instance Applicative (Nuple n) => Applicative (Nuple (S n)) where
    pure n = n :> pure n
    
    f :> fs <*> x :> xs = f x :> (fs <*> xs)

------------------------------------------------

(>==) :: Nuple n x -> (x -> Nuple m y) -> Nuple (n `NatMult` m) y
Zp        >== _ = Zp
(x :> xs) >== f = f x <++> (xs >== f)

------------------------------------------------

instance Foldable (Nuple Z) where
    foldr _ z Zp = z

instance Foldable (Nuple n) => Foldable (Nuple (S n)) where
    foldr f z (x :> xs) = f x (foldr f z xs)

------------------------------------------------

-- mantains the constraint that all elements of the nuple satisfy c
data CNuple :: (Type -> Constraint) -> Nat -> Type where
    Cz  :: CNuple c Z
    (:-) :: c x => x -> CNuple c n -> CNuple c (S n)
infixr 8 :-

instance Show (CNuple Show n) where
    show Cz = "Zp"
    show (x :- xs) = show x ++ " :> " ++ show xs

-------------------------------------------------------------------------

-- mantains the constraint that the size of the nuple satisfies c
type ENuple :: (Nat -> Constraint) -> Type -> Type
data ENuple c x where
    ENuple :: c n => Nuple n x -> ENuple c x
deriving instance Show x => Show (ENuple c x)

nFilter :: (x -> Bool) -> Nuple n x -> ENuple (Gtec n) x
nFilter _ Zp = ENuple Zp
nFilter f (x :> xs) 
    | f x       = case nFilter f xs of
        ENuple k -> ENuple (x :> k)
    | otherwise = case nFilter f xs of
        ENuple k -> ENuple k 

fromList :: [x] -> ENuple UnknownNat x
fromList [] = ENuple Zp
fromList (x:xs) = case fromList xs of
    ENuple k -> ENuple (x :> k) 
