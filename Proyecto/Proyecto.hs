import Aux
frecuenciaLetras :: String -> [Char]
frecuenciaLetras [] = []
frecuenciaLetras palabra = ordenafrecuencia (quitarRepetidos palabra) palabra

construirHuffman :: [Char] -> Arbol Char
construirHuffman [] = Vacio
construirHuffman xs = construir xs