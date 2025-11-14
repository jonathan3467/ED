import Aux
--Te da una lista con cuantas veces aparece cada letra
{-
La que vamos a estar utilizando como ejemplo sera
"si nunca va a amanecer espero que estes tu"
-}
frecuenciasOrdenadas :: String -> [(Char, Int)]
frecuenciasOrdenadas [] = []
frecuenciasOrdenadas palabra = [(c, contar c palabra)
  | c <- ordenafrecuencia (quitarRepetidos palabra) palabra]

-- Te da una lista con el orden de las letras de mayor a menor frecuencia
{-
Aqui utilizaremos la misma frase que sera
"si nunca va a amanecer espero que estes tu"
te debio de regresar " easnucrtivmpoq"
-}
frecuenciaLetras :: String -> [Char]
frecuenciaLetras [] = []
frecuenciaLetras palabra = ordenafrecuencia (quitarRepetidos palabra) palabra


-- Construye nuestro arbol conforme la lista dada del anterior codigo
{-
Aqui le daras la lista anterior que es " easnucrtivmpoq"
-}
construirHuffman :: [Char] -> Arbol Char
construirHuffman [] = Vacio
construirHuffman xs = construir xs

--Dada el arbol anterior te da el codigo de cada letra
{-
Aqui le tendras que dar el arbol anterior osea
" (AB 'R' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' Vacio (AB 'q' Vacio Vacio)) (AB 'o' Vacio Vacio)) (AB 'p' Vacio Vacio)) (AB 'm' Vacio Vacio)) (AB 'v' Vacio Vacio)) (AB 'i' Vacio Vacio)) (AB 't' Vacio Vacio)) (AB 'r' Vacio Vacio)) (AB 'c' Vacio Vacio)) (AB 'u' Vacio Vacio)) (AB 'n' Vacio Vacio)) (AB 's' Vacio Vacio)) (AB 'a' Vacio Vacio)) (AB 'e' Vacio Vacio)) (AB ' ' Vacio Vacio))"  sin las comillas
-}
codigos :: Arbol Char -> [(Char, String)]
codigos Vacio = []
codigos (AB r izq der) 
  | r == 'R' = codigosAux izq "0" ++ codigosAux der "1"
  | otherwise = codigosAux (AB r izq der) ""


--utilizamos el auxiliar, para darle un arbol y el codigo para que nos de la frase o palabra

{-
aqui le tendremo que dar el mismo arbol lo tendras que poner asi
(AB 'R' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' (AB '0' Vacio (AB 'q' Vacio Vacio)) (AB 'o' Vacio Vacio)) (AB 'p' Vacio Vacio)) (AB 'm' Vacio Vacio)) (AB 'v' Vacio Vacio)) (AB 'i' Vacio Vacio)) (AB 't' Vacio Vacio)) (AB 'r' Vacio Vacio)) (AB 'c' Vacio Vacio)) (AB 'u' Vacio Vacio)) (AB 'n' Vacio Vacio)) (AB 's' Vacio Vacio)) (AB 'a' Vacio Vacio)) (AB 'e' Vacio Vacio)) (AB ' ' Vacio Vacio)) "00010000000001100001000001000010000001001100000000001001100110010000000000010010000101000000101000000011010001000000000000101000000010000000000000110000000000000010000010110100010000000010100011000000001000001"

y chan channnn te sale nuestra frase, la puedes inetntar con otra si quieres
pero el decodificar es lo laborioso, no te puedes imaginar cuanto tarde para
hacer el codigo
-}
decodificar :: Arbol Char -> String -> String
decodificar Vacio _ = []
decodificar arbol bits = decodAux arbol bits arbol