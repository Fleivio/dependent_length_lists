module Nat(Nat(..), SNat(..), Fin(..),
    NatSum, NatSub, NatMin, NatMult,
    Lt, Gt, Et,
    Gtc, Ltc, Etc, Gtec, UnknownNat
    ) where

import Data.Kind(Constraint, Type)

data Nat = 
      S Nat 
    | Z
    deriving (Eq, Show)

type SNat :: Nat -> Type
data SNat a where
    Sz :: SNat Z
    Ss :: SNat n -> SNat (S n)

data Fin :: Nat -> Type where
    Fz :: Fin (S n)
    Fs :: Fin n -> Fin (S n)

type family NatSum n m where
    Z   `NatSum` n = n
    S n `NatSum` m = S (n `NatSum` m)

type family NatSub n m where
    n   `NatSub` Z   = n
    Z   `NatSub` n   = Z
    S n `NatSub` S m = n `NatSub` m

type family NatMin n m where
    Z   `NatMin` n      = Z
    S n `NatMin` Z   = Z
    S n `NatMin` S m = S (n `NatMin` m)

type family NatMax n m where
    Z   `NatMax` n      = n
    S n `NatMax` S m = S (n `NatMax` m)

type family NatMult n m where
    Z      `NatMult` n = Z
    S m    `NatMult` n = n `NatSum` (m `NatMult` n) 

---------------------------------------------------

type Et :: Nat -> Nat -> Constraint
type family Et n m where
    Et Z Z = ()
    Et (S n) (S m) = Et n m

type Gt :: Nat -> Nat -> Constraint
type family Gt n m where
    Gt (S n) Z = ()
    Gt (S n) (S m) = Gt n m

type Lt :: Nat -> Nat -> Constraint
type family Lt n m where
    Lt Z (S n) = ()
    Lt (S n) (S m) = Lt n m

---------------------------------------------------

type Etc :: Nat -> Nat -> Constraint
class Etc n m
instance Etc Z Z
instance Etc n m => Etc (S n) (S m)

type Gtc :: Nat -> Nat -> Constraint
class Gtc n m
instance Gtc (S n) Z
instance Gtc n m => Gtc (S n) (S m)

type Gtec :: Nat -> Nat -> Constraint
class Gtec n m
instance Gtec (S n) Z
instance Gtec n n
instance Gtec n m => Gtec (S n) (S m)
instance Gtec n m => Gtec (S n) m

type Ltc :: Nat -> Nat -> Constraint
class Ltc n m
instance Ltc Z (S n)
instance Ltc n m => Ltc (S n) (S m)

type UnknownNat :: Nat -> Constraint
class UnknownNat n
instance UnknownNat n