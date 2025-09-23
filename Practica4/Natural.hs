module Natural (Natural(..), suma_natural, multiplicar) where

data Natural = Cero | S Natural deriving (Eq, Show)

suma_natural:: Natural -> Natural -> Natural
suma_natural Cero n = n
suma_natural (S m) n = suma_natural m (S n)

multiplicar :: Natural -> Natural -> Natural
multiplicar Cero n = Cero
multiplicar (S m) n = suma_natural n (multiplicar m n)