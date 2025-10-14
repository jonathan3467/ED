module Practica6 where
import Aux

{-
Aqui va el 1, primero tenemos en que le damos un arbol y este nos debe de
regresar un entero con la cantidas de vacios que hay, entonces es facil, el
caso base seria cuando este vacio que seria 1, luego que vaya sumando los
lados para ver cuantos vacios hay y yap.
-}

nVacios :: Arbol a -> Int
nVacios Vacio = 1
nVacios (AB r t1 t2) = nVacios t1 + nVacios t2

{-
Aqui va el 2, en este nos piden que reflejemos el arbol pero del intercambiando
los subarboles, esta facil porque lo vimos con el otro ayudante, en el caso
base es cuando este Vacio pues te regrese otro vacio, y ya el recursivo pues
dejamos la raiz tal cual y solamente cambiamos los sub arboles, empezamos por
el derecho y luego el izquierdo
-}

refleja:: Arbol a -> Arbol a
refleja Vacio = Vacio
refleja (AB r t1 t2) = AB r (refleja t2) (refleja t1)

{-
Aqui va la 3, este fue uno de los más dificiles, aqui como vamos a tener que
comparar utilizamos el Ord, y como caso base cuando esta vacio pues que lance
error, luego si solamente esta la raiz, pues el minimo sera la raiz, luego
vamos con lo pesado hacemos guardas, el primero si el sub arbol izquierdo esta
vacio pues que cheque si r es menor que recursion de t2, si si pues que te
regrese r, si no que se siga con t2, ahora en caso de que t2 sea vacio pues se
hace lo mismo pero con el sub arbol izquierdo, y al final el otherwise es sin
ninguna de las dos se cumple, osea si estans los dos sub arboles, lo que
hacemos es verificar si la raiz es menor al minimo que se hace en toda la
recursion de t1 y de t2, si si que de r, si no que se siga con los sub
arboles hast encontrar al menor
-}

minimo :: (Ord a) => Arbol a -> a
minimo Vacio = error "no hay nada"
minimo (AB r Vacio Vacio) = r
minimo (AB r t1 t2)
  | t1 == Vacio =
      if r < minimo t2 then r else minimo t2
  | t2 == Vacio =
      if r < minimo t1 then r else minimo t1
  | otherwise =
      if r < minimo t1 && r < minimo t2 then r
      else if minimo t1 < minimo t2 then minimo t1
      else minimo t2


{-
Aqui llamamos una nueva funcion recorrido para asi poder pasar los test, donde
primero se tiene que poner recorrido luego el arbol y luego el tipo de orden
que queremos y llamamos nuestros auxiliares
-}

recorrido :: Arbol a -> TipoRecorrido -> [a]
recorrido arbol InOrden = inorden arbol
recorrido arbol PreOrden = preorden arbol
recorrido arbol PosOrden = postorden arbol

{-
Esta es la 5, aqui vamos a utilizar abs que es para tener el valor absoluto
esto es por si en la diferencia de alturas nos sale algun negativo lo vuelva
positivo para asi poder saber si si diferencia de altura es de 1 o más para
saber si es balanceado
-}

esBalanceado :: Arbol a -> Bool
esBalanceado Vacio = True
esBalanceado (AB r t1 t2) = abs (altura t1 - altura t2) <= 1 &&
   esBalanceado t1 && esBalanceado t2

{-
Esta es la 6, aqui lo unico que hacemos es conforme nos den la lista pues le
quitamos la cabeza y la insertamos en el arbol con el auxiliar, y asi hasta que
no quede ningun elemento de la cola
-}

listaArbol :: Ord a => [a] -> Arbol a
listaArbol [] = Vacio
listaArbol (x:xs) = insertarTodos xs (AB x Vacio Vacio)