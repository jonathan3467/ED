module Mati (Matrioska(..), tamanio) where
data Matrioska = Mati | Cont Matrioska deriving (Eq, Show)

tamanio :: Matrioska -> Int
tamanio Mati = 1 
tamanio (Cont m) = 1 + tamanio m  