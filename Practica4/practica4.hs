import Natural
import Mati

-- es la primera toma un entero y lo lleva a la forma natural que hemos visto en clase, con su caso base en caso de que nos den el cero y un error en caso de que nos den un negativo
a_natural :: Int -> Natural
a_natural n = 
  if n == 0 then Cero
  else if n > 0 then S (a_natural (n - 1))
  else error "los negativos no se pueden"

-- para pasar de un numero natural a un entero el, igual que el anterior pero al reves con el mismo caso base
a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S n) = 1 + a_entero n

-- el 3 para la potencia, aqui utlizamos multiplicacion y suma que estan en el auxiliar, esto se hace mediante mucha recursion,  con dos casos bases explicados en el readme

potenciaNat n Cero = S Cero
potenciaNat Cero (S _) = error "esto no esta definido"
potenciaNat n (S m) = multiplicar n (potenciaNat n m)

-- el 4 para el factorial, aqui igual utilizamos la multiplicacion y suma con el caso base en dado caso que nos den  cero

facNat :: Natural -> Natural
facNat Cero = S Cero
facNat (S n) = multiplicar (S n) (facNat n)

-- el 5 para mati, esto es lo mismo de la  clase, utilizando tamanio que nos lo  dio en laboratorio y es auxiliar
mayorIgual :: Matrioska -> Matrioska -> Bool
mayorIgual m1 m2 = tamanio m1 >= tamanio m2

-- el 6 para el aplan este se estara explicado en el Readme pero es para que te den todo lo que esta adentro de la matrioska que le diste

aplana :: Matrioska -> [Matrioska]
aplana Mati     = [Mati]
aplana (Cont m) = Cont m : aplana m

