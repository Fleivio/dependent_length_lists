module Op(Op(..), app, idOp, foldOp, (>-), (-<), (>>-), (-<<), (>$)) where

import Nat
import Nuple
import Data.Kind(Type)

type Op :: Nat -> Nat -> Type -> Type -> Type
data Op n m a b where
    Op :: (Nuple n x -> Nuple m y) -> Op n m x y

(>-) :: Op n m x y -> Op m o y z -> Op n o x z
Op f >- Op g = Op (g . f)
infixr 1 >-

(>$) :: (a -> b) -> a -> b
(>$) = ($)
infixr 1 >$

(-<) :: Op o n y z -> Op m o x y -> Op m n x z
Op f -< Op g = Op (f . g)
infixr 1 -<

app :: Op n m x y -> Nuple n x -> Nuple m y
app (Op f) = f

(-<<) :: Op n m x y -> Nuple n x -> Nuple m y
o -<< x = app o x
infixr 1 -<<

(>>-) :: Nuple n x -> Op n m x y -> Nuple m y
(>>-) = flip (-<<)
infixl 1 >>-


idOp :: Op n n a a
idOp = Op id

foldOp :: (a -> a -> a) -> Op n (S Z) a a
foldOp f = Op $ \xs -> foldr1 f xs :> Zp

--------------------------------------------------
instance Functor (Op n m a) where
    fmap f (Op g) = Op (fmap f . g)

----------------------------------------------------

instance Applicative (Nuple m) => Applicative (Op n m a) where

    pure a1 = Op $ \_ -> pure a1

    Op f <*> Op g = Op $ \a -> f a <*> g a


-- extend ::  (m `Gt` n, n `Gt` k) => 
--         {-Original Operarion-} Op n x ->
--         {-Selectors-}          CNuple (Gtec m) k ->
--         {-Total Size-}         SNat m -> 
--         {-Out Operation-}      Op m x
-- extend = undefined


