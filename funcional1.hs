{- 1.1. Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3 -}

esMultiploDeTres :: Integer -> Bool
esMultiploDeTres x = x `mod` 3 == 0

-- 1.2. Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = x `mod` y == 0 

-- 1.3. Definir la función cubo/1, devuelve el cubo de un número.

cubo :: Integer -> Integer
cubo x = x * x * x
-- otra variante (x^3)

-- 1.4. Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.

area :: Float -> Float -> Float
area base altura = base * altura

-- 1.5. Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100).
--Nota: Resolverlo reutilizando la función esMultiploDe/2

esBisiesto :: Integer -> Bool
esBisiesto anio = esMultiploDe anio 400 || esMultiploDe anio 4 && not (esMultiploDe anio 100)

-- 1.6. Definir la función celsiusToFahr/ 1, pasa una temperatura en grados Celsius a grados Fahrenheit.

celsiusToFahr :: Float -> Float
celsiusToFahr x = x * 1.8 + 32

-- 1.7. Definir la función fahrToCelsius/ 1, la inversa de la anterior.

fahrToCelsius :: Float -> Float
fahrToCelsius x = (x - 32) / 1.8

-- 1.8. Definir la función haceFrioF/ 1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius.

haceFrioF :: Float -> Bool
haceFrioF x = (fahrToCelsius x)<8
-- Otra alternativa, retornando un String
--haceFrioString :: Float -> String
--haceFrioString gradoF | (fahrToCelsius gradoF) < 8 = "Hace Frío" 
--                | otherwise  = "No Hace Frío"

-- 1.9. Definir la función mcm /2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula.
-- m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)}
-- Nota: Se puede utilizar gcd.

mcm :: Integer -> Integer -> Integer
mcm a b = (a * b) `div` (gcd a b)

{- 1.10. Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm.
P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm.
A partir de estos tres números, podemos obtener algunas conclusiones.
Definir estas funciones:
1.10. a. dispersion , que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min,
que estamos usando).
1.10. b. diasParejos , diasLocos y diasNormales reciben los valores de los tres días. Se dice que
son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, y que son
días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de
30 cm, y grande si es de más de un metro.
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas.
-}

dispersion :: Integer -> Integer -> Integer -> Integer
dispersion med1 med2 med3 = max med1 (max med2 med3) - min med1 (min med2 med3)

diasParejos :: Integer -> Integer -> Integer -> Bool
diasParejos a b c = (dispersion a b c) < 30

diasLocos :: Integer -> Integer -> Integer -> Bool
diasLocos a b c = (dispersion a b c) > 100

diasNormales :: Integer -> Integer -> Integer -> Bool
diasNormales a b c = (not (diasParejos a b c)) && (not (diasLocos a b c))

{- 1.11. En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros.
P.ej. 2 metros -> 600 kg, 5 metros -> 1300 kg.
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica.
Para esta situación:
Definir la función pesoPino , recibe la altura de un pino y devuelve su peso. Definir la función esPesoUtil , recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Definir la función sirvePino , recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición. -}

pesoPino :: Integer -> Integer
pesoPino altura = if altura <= 3 
                  then altura * 100 * 3 
                  else 900 + ((altura - 3) * 100 * 2)

esPesoUtil :: Integer -> Bool
esPesoUtil peso = (peso >= 400) && (peso <= 1000)

sirvePino :: Integer -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

 {- 1.12. Desafío Café con Leche: Implementar la función esCuadradoPerfecto /1, sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar
al 3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de
recursividad van a tener que usar. -}

esCuadradoPerfecto :: Integer -> Bool
esCuadradoPerfecto numero = esIgualCuadrado numero 0

esIgualCuadrado :: Integer -> Integer -> Bool
esIgualCuadrado numero raiz = if(raiz^2 < numero)
                                then esIgualCuadrado numero (raiz+1)
                                else (raiz^2 == numero)
main = do
  print("esMultiploDeTres 9")
  print(esMultiploDeTres 9)
  print("esMultiploDe 12 3")
  print(esMultiploDe 12 3)
  print("cubo 3")
  print(cubo 3)
  print("area 10 20")
  print(area 10 20)
  print("esBisiesto 2020")
  print(esBisiesto 2020)
  print("celsiusToFahr 30")
  print(celsiusToFahr 30)
  print("fahrToCelsius 30")
  print(fahrToCelsius 30)
  print("haceFrioF 30")
  print(haceFrioF 30)
  print("mcm 12 3")
  print(mcm 12 3)
  print("diasNormales 100 200 300")
  print(diasNormales 100 200 300)
  print("pesoPino 100")
  print(pesoPino 100)
  print("esCuadradoPerfecto 4")
  print(esCuadradoPerfecto 4)