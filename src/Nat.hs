module Nat(Nat(..), SNat(..), 
    NatSum, NatMin, NatMult, Lt, Gt) where

import Data.Kind(Constraint, Type)

data Nat = 
      Succ Nat 
    | Zero
    deriving (Eq, Show)

type SNat :: Nat -> Type
data SNat a where
    Szero :: SNat Zero
    Ssucc :: SNat n -> SNat (Succ n)

type family NatSum n m where
    Zero   `NatSum` n = n
    Succ n `NatSum` m = Succ (n `NatSum` m)

type family NatMin n m where
    Zero   `NatMin` n      = Zero
    Succ n `NatMin` Succ m = Succ (n `NatMin` m)

type family NatMax n m where
    Zero   `NatMax` n      = n
    Succ n `NatMax` Succ m = Succ (n `NatMax` m)

type family NatMult n m where
    Zero      `NatMult` n = Zero
    Succ m    `NatMult` n = n `NatSum` (m `NatMult` n) 

type family Lt n m :: Constraint where
    Lt Zero (Succ n) = ()
    Lt (Succ n) (Succ m) = Lt n m

type family Gt n m :: Constraint where
    Gt (Succ n) Zero = ()
    Gt (Succ n) (Succ m) = Gt n m

-- type  Gt :: Nat -> Nat -> Constraint
-- class Gt n m
-- instance Gt (Succ n) Zero
-- instance Gt n m => Gt (Succ n) (Succ m)

-- type  Lt :: Nat -> Nat -> Constraint
-- class Lt n m
-- instance Lt Zero (Succ n)
-- instance Lt n m => Lt (Succ n) (Succ m)
