module Op(Op'(..), Op, app, idOp, (>>-), (-<<), (>>->), (<-<<)) where

import Nat
import Nuple
import Data.Kind(Type)

type Op' :: Nat -> Nat -> Type -> Type -> Type
data Op' n m a b where
    Op :: (Nuple n x -> Nuple m y) -> Op' n m x y

type Op :: Nat -> Type -> Type
type Op n x = Op' n n x x 

(>>-) :: Op' n m x y -> Op' m o y z -> Op' n o x z
Op f >>- Op g = Op (g . f)

(-<<) :: Op' m o y z -> Op' n m x y -> Op' n o x z
Op f -<< Op g = Op (f . g)

app :: Op' n m x x -> Nuple n x -> Nuple m x
app (Op f) = f

(<-<<) :: Op' n m x x -> Nuple n x -> Nuple m x
o <-<< x = app o x
infixr 1 <-<<

(>>->) :: Nuple n x -> Op' n m x x -> Nuple m x
x >>-> o = app o x
infixl 1 >>->

idOp :: Op n a
idOp = Op id

--------------------------------------------------
instance Functor (Op' n m a) where
    fmap f (Op g) = Op (fmap f . g)

----------------------------------------------------

instance Applicative (Nuple m) => Applicative (Op' n m a) where

    pure a1 = Op $ \_ -> pure a1

    Op f <*> Op g = Op $ \a -> f a <*> g a


-- gen ::  (m `Gt` n) => 
--         {-Original Operarion-} Op n x ->
--         {-Selectors-}          CNuple (Gtec m) n ->
--         {-Total Size-}         SNat m -> 
--         {-Out Operation-}      Op m x
-- gen = undefined


