module Aux (Arbol(..), TipoRecorrido(..), inorden, preorden, postorden, altura, inserta, insertarTodos) where
data Arbol a = Vacio | AB a (Arbol a) (Arbol a) deriving (Eq, Ord, Show)
data TipoRecorrido = InOrden | PreOrden | PosOrden deriving (Eq, Show)

{-
Esta es la 4, fue uno de las más faciles porque algunas las vimos en clase,
aqui en Inorden pues primero hacemos la recursion con el lado izquierdo, lo
concatenamos con la raiz y lo concatenamos con la recursion de la derecha, y
utilizamos lo mismo en todos solamente en diferente orden
-}

inorden :: Arbol a -> [a]
inorden Vacio = []
inorden (AB r Vacio Vacio) = [r]
inorden (AB r t1 t2) = inorden t1 ++ [r] ++ inorden t2

preorden :: Arbol a -> [a]
preorden Vacio = []
preorden (AB r Vacio Vacio) = [r]
preorden (AB r t1 t2) = [r] ++ preorden t1 ++ preorden t2

postorden :: Arbol a -> [a]
postorden Vacio = []
postorden (AB r Vacio Vacio) = [r]
postorden (AB r t1 t2) = postorden t1 ++ postorden t2 ++ [r]

{-
Estes e el auxiliar para el 5, en este caso necesitamos el de altura para poder
saber si es balanceado el arbol
-}

altura :: Arbol a -> Int
altura Vacio = 0
altura (AB r t1 t2) = 1 + max (altura t1) (altura t2)

{-
Funcion auxiliar para la ultima, aqui vamos a utilizar la funcion inserta que
nuestro buen amigo Irvin hizo, esta nos ayudara para saber si insertar a la
derecha o a la izquierda del arbol, pero tambien necesitamos otro para insertar
los los elementos de la lista, para luego en la funcion principal poner como
raiz a la cabeza de la lista si no no pasa el test
-}

inserta :: Ord a => a -> Arbol a -> Arbol a
inserta n Vacio = AB n Vacio Vacio
inserta n (AB r t1 t2)
    | n == r = AB r t1 t2
    | n < r  = AB r (inserta n t1) t2
    | n > r  = AB r t1 (inserta n t2)

insertarTodos :: Ord a => [a] -> Arbol a -> Arbol a
insertarTodos [] arbol = arbol
insertarTodos (y:ys) arbol = insertarTodos ys (inserta y arbol)
