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

--type Ejercicio = Gimnasta -> Gimnasta

-- 2.a) La cinta es una de las máquinas más populares entre los socios que quieren perder peso. 
-- Los gimnastas simplemente corren sobre la cinta y queman calorías en función de la velocidad promedio alcanzada 
-- (quemando 10 calorías por la velocidad promedio por minuto).

type Tiempo = Number
type Velocidad = Number
type Peso = Number
type Inclinacion = Number

cinta :: Number -> Tiempo -> (Gimnasta -> Gimnasta)
cinta velocidad tiempo = quemarCalorias (10 * velocidad * tiempo) 

-- La cinta puede utilizarse para realizar dos ejercicios diferentes:
-- 2.a.i) La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.

caminata :: Tiempo -> Gimnasta -> Gimnasta
caminata = cinta 5  -- en este caso NO necesito el parametro del tiempo para calcular 

-- OJO entonces cuando quiera hacer una caminata le voy a tener que pasar el tiempo porque es un parametro que necesita la cinta
-- independientemente de que este o no esta explicitamente usado!! (en este caso use Point Free!!)

-- 2.a.ii) El pique arranca en 20 km/h y cada minuto incrementa la velocidad en 1 km/h, 
-- con lo cual la velocidad promedio depende de los minutos de entrenamiento.

pique :: Tiempo -> Gimnasta -> Gimnasta
pique tiempo = cinta (tiempo `div` 2 + 20) tiempo   -- en este caso si es necesario agregar el tiempo (esta explicito)

-- OJO entonces cuando quiera hacer una caminata le voy a tener que pasar el tiempo porque es un parametro que necesita la cinta

-- 2.b) Las pesas son el equipo preferido de los que no quieren perder peso, sino ganar musculatura. 
-- Una sesión de levantamiento de pesas de más de 10 minutos hace que el gimnasta gane una tonificación 
-- equivalente a los kilos levantados. Por otro lado, una sesión de menos de 10 minutos es demasiado corta, 
-- y no causa ningún efecto en el gimnasta.

pesas :: Peso -> Tiempo -> Gimnasta -> Gimnasta
pesas peso tiempo gimnasta 
    | tiempo > 10 = tonificar peso gimnasta
    | otherwise   = gimnasta

-- 2.c) La colina es un ejercicio que consiste en ascender y descender sobre una superficie inclinada y 
-- quema 2 calorías por minuto multiplicado por la inclinación con la que se haya montado la superficie.

colina :: Inclinacion -> Tiempo -> Gimnasta -> Gimnasta
colina inclinacion tiempo = quemarCalorias (2 * tiempo * inclinacion) 