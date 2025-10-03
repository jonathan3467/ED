module Practica5 where
import Data.Char (toUpper)
{-
Este es el primero, fue el más facil porque fue como las recursiones que hemos echo en clase, solamente que aqui utilizamos el toUpper que la verdad no sabia ni para que servia ni sabia que lo podiamos utilizar hasta que vi el archivo de practica 5, y ahi investigue en el libro para que servia en la pág 74, y con eso fue facil
-}
hollerBack :: String -> String
hollerBack [] = []
hollerBack (x:xs) = toUpper x : hollerBack xs

{-
Este es el 2, la verdad fue el que más me costo porque no sabia como hacerle, primero pense en crear una funcion auxiliar para que quitara el ultimo de la lista despues de igualarlo con la cabeza, hasta que le che una leida al libro y ahi encontre el "last" y el "init" en las primeras paginas, y las decidi usar despues me parecio muy facil
-}
palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo [x] = True
palindromo (x:xs) = x == last xs && palindromo (init xs)

{-
este es el 3 me parecio algo sencillo ya que cuando lo lei se me vino la idea a la mente de como hacerle que era facil solamente utilizando a n como el numero que nos pide y ya restandole uno despues de cada recursión
-}

replica :: Int -> Int -> [Int]
replica _ 0 = []
replica x 1 = [x]
replica x n = x: replica x (n-1)

{-
este es el 4 fue de los más complicados, cuando lo lei tuve la idea pero en los casos base fue em los que sufri sobretodo en el de if n< 0 ahi fue en el que sufri aunque en si no es necesario ponerlo porque pues haskell no te deja poner numeros negativos pero siempre hay que pensar en todos los posibles casos bases y ya lo demas estuvo algo facil por asi decirlo solamente que si me rompi la cabeza pensando si te pedia un numero mas grande que la lista, pero al final todo resulto
-}

recuperaElemento :: [Int] -> Int -> Int
recuperaElemento  [] _ = error "La lista esta vacia"
recuperaElemento (x:_) 0 = x
recuperaElemento  (_:xs) n =
  if n < 0 then error "Pon un numero mayor o igual a 0"
else recuperaElemento xs (n - 1) 

{-
este es el 5, fue uno de los más faciles tambien, aqui mi unico error era que yo estaba poniendole (xs : [x]) pero despues me di cuenta que no se podia entonces decidi con concatenar que se lo escuche a un chavo de nuestro salon y era algo que ya habiamos ocupado, y en vez de hacer un auxiliar pues lo hice aqui mismo con el ++ 
-}

rota :: [a] -> Int -> [a]
rota xs 0 = xs
rota [] _ = []
rota (x:xs) n = rota (xs ++ [x]) (n-1)


{-
Este es el 6 tambien fue uno de los más sencillos y a la vez no, porque la idea se me vino muy facil utilizar el mod para ver si es par y si no pues que ocurra lo otro utilizando if, lo unico complicado fue poner el div ya que yo pretendia poner el / pero me marcaba error entonces tuve que buscar en el libro tal cual la palabra division y me aparecio ahi el div 
-}

extranio :: Int -> [Int]
extranio 1 = [1]
extranio n =
  if n `mod` 2 == 0
  then n : extranio (n `div` 2)
  else n : extranio (n * 3 +1)