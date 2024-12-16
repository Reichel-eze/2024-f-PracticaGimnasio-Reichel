module Library where
import PdePreludat

-- 1) Modelar a los Gimnastas y las operaciones 
-- necesarias para hacerlos ganar tonificación y 
-- quemar calorías considerando que por cada 500 calorías 
-- quemadas se baja 1 kg de peso.

data Gimnasta = UnGimnasta {
    peso :: Number,
    tonificacion :: Number
}deriving(Show , Eq)

-- Instancio un gimnasta para ir probando
eze :: Gimnasta
eze = UnGimnasta 80 10

tonificar :: Number -> Gimnasta -> Gimnasta
tonificar valorGanadoDeTonificacion gimnasta = gimnasta {tonificacion = tonificacion gimnasta + valorGanadoDeTonificacion}

--quemarCalorias :: Number -> Gimnasta -> Gimnasta
--quemarCalorias calorias gimnasta = gimnasta {peso = peso gimnasta - relacionCaloriasPeso calorias} 

quemarCalorias :: Number -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta = gimnasta {peso = peso gimnasta - calorias `div` 500} 

-- "Por cada 500 calorias quemadas -----> se baja 1 kg de peso" (No es necesario hacer una funcion aparte)
--relacionCaloriasPeso :: Number -> Number
--relacionCaloriasPeso numero = numero `div` 500 

-- 2) Modelar los siguientes ejercicios del gimnasio

type Ejercicio = Tiempo -> Gimnasta -> Gimnasta

-- LA CINTA EN SI NO ES UN EJERCICIO!!
-- 2.a) La cinta es una de las máquinas más populares entre los socios que quieren perder peso. 
-- Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad promedio alcanzada 
-- (quemando 10 calorías por la velocidad promedio por minuto).

type Tiempo = Number
type Velocidad = Number
type Peso = Number
type Grados = Number

--       Number -> Ejercicio   
cinta :: Number -> Tiempo -> (Gimnasta -> Gimnasta)
cinta velocidad tiempo = quemarCalorias (10 * velocidad * tiempo) 

-- La cinta puede utilizarse para realizar dos ejercicios diferentes:
-- 2.a.i) La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.

--          Ejercicio
caminata :: Tiempo -> Gimnasta -> Gimnasta
caminata = cinta 5  -- en este caso NO necesito el parametro del tiempo para calcular 

-- OJO entonces cuando quiera hacer una caminata le voy a tener que pasar el tiempo porque es un parametro que necesita la cinta
-- independientemente de que este o no esta explicitamente usado!! (en este caso use Point Free!!)

-- 2.a.ii) El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, 
-- con lo cual la velocidad promedio depende de los minutos de entrenamiento.

--       Ejercicio
pique :: Tiempo -> Gimnasta -> Gimnasta
pique tiempo = cinta (tiempo `div` 2 + 20) tiempo   -- en este caso si es necesario agregar el tiempo (esta explicito)

-- OJO entonces cuando quiera hacer una caminata le voy a tener que pasar el tiempo porque es un parametro que necesita la cinta

-- 2.b) Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. 
-- Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación 
-- equivalente a los kilos levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, 
-- y no causa ningún efecto en el gimnasta.

--       Peso -> Ejercicio
pesas :: Peso -> Tiempo -> Gimnasta -> Gimnasta
pesas peso tiempo gimnasta 
    | tiempo > 10 = tonificar peso gimnasta
    | otherwise   = gimnasta

-- con id
pesas' :: Peso -> Tiempo -> (Gimnasta -> Gimnasta)
pesas' peso tiempo  
    | tiempo > 10 = tonificar peso 
    | otherwise   = id

-- 2.c) La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y 
-- quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.

--        Grados -> Ejercicio
colina :: Grados -> Tiempo -> (Gimnasta -> Gimnasta)
colina inclinacion tiempo = quemarCalorias (2 * tiempo * inclinacion) 

-- Los gimnastas más experimentados suelen preferir otra versión de este ejercicio: la montaña, 
-- que consiste en 2 colinas sucesivas (asignando a cada una la mitad del tiempo total), donde 
-- la segunda colina se configura con una inclinación de 5 grados más que la inclinación de la primera. 
-- Además de la pérdida de peso por las calorías quemadas en las colinas, la montaña incrementa en 3 unidades
-- la tonificación del gimnasta

--          Grados -> Ejercicio
montania :: Grados -> Tiempo -> (Gimnasta -> Gimnasta)
montania inclinacion tiempo = tonificar 3 . colina (inclinacion + 5) (tiempo / 2) . colina inclinacion (tiempo / 2) 
-- 1ero. Quiero que el gimnasta realize la 1era colina 
-- 2dos. Al gimnasta resultante del primero quieo que realice una 2da colina pero con una modificacion en la inclinacion
-- 3ero. Por ultimo quiero que el gimnasta resultante se tonifique en 3 unidades

-- 3) Dado un gimnasta y una Rutina de Ejercicios, representada con la siguiente estructura:

data Rutina = UnaRutina {
    nombre :: String,
    duracionTotal :: Number,
    ejercicios :: [Ejercicio]
} deriving (Show, Eq)

-- Implementar una función realizarRutina, que dada una rutina y un gimnasta retorna el gimnasta resultante de 
-- realizar todos los ejercicios de la rutina, repartiendo el tiempo total de la rutina en partes iguales. 
-- Mostrar un ejemplo de uso con una rutina que incluya todos los ejercicios del punto anterior.

realizarRutina :: Gimnasta -> Rutina -> Gimnasta
realizarRutina gimnasta rutina = 
    foldl (\gimnasta ejercicio -> ejercicio (tiempoParaEjercicio rutina) gimnasta) gimnasta (ejercicios rutina)  
--          "funcion transformadora"            
-- Lo que hace la funcion, es dado un gimnasta y un ejercicio --> realizo el ejercicio con un tiempo sobre el gimnasta
-- Lo que hay dentro de la funcion es la funcion individual para cada ejercicio analizado de la lista de ejercicios

tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = (div (duracionTotal rutina) . length . ejercicios) rutina

tiempoParaEjercicio' :: Rutina -> Tiempo
tiempoParaEjercicio' rutina = duracionTotal rutina `div` length (ejercicios rutina)

-- Los ejemplos son para darte cuenta si vas por el buen camino

rutina1 :: Rutina
rutina1 = UnaRutina "rutina 1" 30 [caminata, colina 5, pesas 4, colina 2]

rutina2 :: Rutina
rutina2 = UnaRutina "rutina 2" 30 [caminata, pique]

-- 4) Definir las operaciones necesarias para hacer las siguientes consultas a partir de una lista de rutinas:

-- 4.a) ¿Qué cantidad de ejercicios tiene la rutina con más ejercicios?

mayorcantidadDeEjerciciosDeLaMayor :: [Rutina] -> Number
mayorcantidadDeEjerciciosDeLaMayor = maximum . map (length . ejercicios)

cantidadDeEjercicios :: Rutina -> Number
cantidadDeEjercicios = length . ejercicios 

-- 4.b) ¿Cuáles son los nombres de las rutinas que hacen que un gimnasta dado gane tonificación?

nombresDeRutinasTonificantes :: Gimnasta -> [Rutina] -> [String]
nombresDeRutinasTonificantes gimnasta = map nombre . filter (ganoTonificacion' gimnasta) 

ganoTonificacion :: Gimnasta -> Rutina -> Bool
ganoTonificacion gimnasta rutina = tonificacion gimnasta < tonificacion (realizarRutina gimnasta rutina) 

-- Con COMPOSICION
-- Y aprovechando que NO HAY EFECTO puedo usar el gimnasta dos veces!! (una para genera una copia difertente del mismo al realizar la rutina y otra para consultar un dato del gimnasta inicial)
ganoTonificacion' :: Gimnasta -> Rutina -> Bool
ganoTonificacion' gimnasta = (> tonificacion gimnasta ) . tonificacion . realizarRutina gimnasta
-- 1ero. Realizo la rutina 
-- 2dos. Me fijo la tonificacion que tendria el gimnasta luego de realizar la rutina (la persona hipotetica "copia mejorada del original PERO NO ES EL ORIGINAL PORQUE NO HAY EFECTO")
-- 3ero. Verifico que la tonifacion luego de realizar la rutina sea mayor a la tonificacion que tenia el 
-- gimnasta antes de realizarla

-- 4.c) ¿Hay alguna rutina peligrosa para cierto gimnasta? Decimos que una rutina es peligrosa para 
-- alguien si lo hace perder más de la mitad de su peso.

hayRutinaPeligrosa :: Gimnasta -> [Rutina] -> Bool
hayRutinaPeligrosa gimnasta = any (esPeligrosa gimnasta)

esPeligrosa :: Gimnasta -> Rutina -> Bool
esPeligrosa gimnasta = (< peso gimnasta `div` 2) . peso . realizarRutina gimnasta
-- Idem que el anterior pero con la idea del peso si disminuyo
