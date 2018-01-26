module Matrix

import Data.Vect

data Compose: f -> g -> c -> Type where
     Composer: f (g c) -> Compose f g c

implementation (Functor a, Functor b) => Functor (Compose a b) where
    map func (Composer x) = Composer (map (map func) x)

(Applicative f, Applicative g) => Applicative (Compose f g) where
    pure a = Composer $ (pure . pure) a
    (Composer f) <*> (Composer x) = Composer (liftA2 (<*>) f x)

Matrix: (row: Nat) -> (col: Nat) -> (a: Type) -> Type
Matrix row col a  = Compose (Vect row) (Vect col) a

addMatrix: Num a => (Matrix rows cols a) -> (Matrix rows cols a) -> (Matrix rows cols a)
addMatrix = liftA2 (+)

xs: Vect 2 (Vect 3 Int)
xs = [[1,2,3], [4, 5, 6]]

composedXs: Matrix 2 3 Int
composedXs = Composer xs