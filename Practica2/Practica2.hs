-- 2 esto es para el saludo con tu nombre poniendolo entre comillas

   say :: String -> String 
   say nombre =  "hola " ++ nombre 


-- 3 esto es para calcular propina
   propina :: Float -> Float
   propina x = x * 0.10 

-- 4 esto es para calcular el menor
   menor :: Int -> Int -> Int -> Int
   menor x y z  = min x (min y z) 

-- 5 esto es para decidir, si escribes True te da hola, si escribes False te da haskell
   decide :: Bool -> String
   decide x = if x then "Hola" else "Haskell" 


--  6 descendiente, da true si se colocan de manera mayor a menor, en otro caso da false
   descendiente :: Int -> Int -> Int -> Int -> Bool
   descendiente x y z w = x > y && y > z && z > w 

-- 7 para calcular si es divisible, aqui utlice show para cambiar los int a texto para poder imprimirlos

   divisible :: Int -> Int -> String
   divisible x y = if y == 0 then "No se puede dividir entre cero"
   else if x `mod` y == 0 then show x ++ " Es divisible por " ++ show y
   else show x ++ " No es divisible por " ++ show y 


-- 8 para calcular la hipotenusa de un triangulo con valores con punto
   hipotenusa :: Float -> Float -> Float
   hipotenusa b h = sqrt (b*b + h*h) 

-- 9 sacar la pendiente, asignas valores flotantes y te regresa uno flotante
   pendiente :: (Float, Float) -> (Float, Float) -> Float
   pendiente (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1) 

-- 10 para medir la distancia entre dos puntos con punto decimal
   distancia :: (Float, Float) -> (Float, Float) -> Float
   distancia (x1,y1) (x2,y2) = sqrt ((x2 -x1) ^2 + (y2 - y1) ^2)

-- 11 Cuadrados recibe int y te saca los cuadrados de eso
   cuadrados :: (Int, Int, Int) -> (Int, Int, Int)
   cuadrados (x, y, z) = (x * x, y * y, z * z) 
