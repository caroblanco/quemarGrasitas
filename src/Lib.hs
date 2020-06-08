module Lib where
import Text.Show.Functions

data Gimnasta = Gimnasta {
    nombre :: String,
    edad :: Float,
    peso:: Float,
    coeficienteTonificacion :: Float
} deriving(Show)


pancho = Gimnasta {nombre = "Francisco", edad= 40.0,peso= 120.0, coeficienteTonificacion= 1.0}
andres = Gimnasta {nombre ="Andy", edad= 22.0, peso= 80.0, coeficienteTonificacion= 6.0}

type Ejercicio = Float -> Gimnasta -> Gimnasta

relax :: Ejercicio
relax minutos gimnasta = gimnasta

----------------1
saludable :: Gimnasta -> Bool
saludable gimnasta = (not.obeso) gimnasta && tonificacionMayorA gimnasta

obeso :: Gimnasta -> Bool
obeso = (>100).peso

tonificacionMayorA ::  Gimnasta -> Bool
tonificacionMayorA gimnasta = ((>5).coeficienteTonificacion) gimnasta

----------------2

quemarCalorias :: Gimnasta -> Float -> Gimnasta
quemarCalorias gimnasta calorias
    | obeso gimnasta = cambiarPeso (restarPeso (calorias / 150)) gimnasta
    | (not.obeso) gimnasta && edadMayorA gimnasta && calorias > 200 = cambiarPeso (restarPeso 1) gimnasta 
    | otherwise = cambiarPeso (restarPeso (calorias / ((peso gimnasta) * (edad gimnasta)))) gimnasta

edadMayorA :: Gimnasta -> Bool
edadMayorA = (>30).edad

cambiarPeso :: (Float->Float) -> Gimnasta -> Gimnasta
cambiarPeso f gimnasta = gimnasta {peso = f (peso gimnasta)}

restarPeso resto peso = peso - resto

--------------3

caminataEnCinta :: Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (calorias minutos)

calorias :: Float -> Float
calorias = (*1).(*5)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (calorias2 minutos)

calorias2 minutos = (1* (6 + (6+ minutos/5)) / 2 * minutos)

pesas :: Float -> Ejercicio
pesas peso minutos gimnasta 
    | minutos > 10 = cambiarTonificacion (+(peso/10)) gimnasta
    | otherwise = gimnasta

cambiarTonificacion :: (Float -> Float) -> Gimnasta -> Gimnasta
cambiarTonificacion f gimnasta = gimnasta {coeficienteTonificacion = f (coeficienteTonificacion gimnasta)}

colina :: Float -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (calorias3 minutos inclinacion)

calorias3 :: Float -> Float -> Float
calorias3 minutos inclinacion = 2*minutos*inclinacion

montania :: Float -> Ejercicio
montania inclinacion1 minutos = cambiarTonificacion (\peso -> peso+1) . colina inclinacion1 (minutos/2). colina (inclinacion1 + 3) (minutos/2)


-----------4
data Rutina = UnaRutina {
    nombreRu :: String,
    duracion :: Float,
    listaEjs :: [Ejercicio]
} deriving (Show)

duracionEj :: Rutina -> Float
duracionEj rutina = (duracion rutina) / ((fromIntegral.length.listaEjs) rutina)

hacerRutina :: Rutina -> Gimnasta -> Gimnasta
hacerRutina rutina gimnasta = realizar (listaEjs rutina) (duracionEj rutina) gimnasta 

realizar :: [Ejercicio] -> Float -> Gimnasta -> Gimnasta
realizar [] _ gimnasta = gimnasta
realizar (x:xs) duracion gimnasta = realizar xs duracion (ejercitar duracion gimnasta x) 

hacerRutina2 :: Rutina -> Gimnasta -> Gimnasta
hacerRutina2 rutina gimnasta = realizar2 (listaEjs rutina) (duracion rutina) gimnasta

realizar2 :: [Ejercicio] -> Float -> Gimnasta -> Gimnasta
realizar2 lista duracion gimnasta = foldl (ejercitar duracion) gimnasta lista


ejercitar :: Float -> Gimnasta -> Ejercicio -> Gimnasta
ejercitar duracion gimnasta ejercicio = ejercicio duracion gimnasta


resumenRutina :: Rutina -> Gimnasta -> (String, Float,Float)
resumenRutina rutina gimnasta = (nombreRu rutina, diferenciaAdquirida peso rutina gimnasta, diferenciaAdquirida coeficienteTonificacion rutina gimnasta)

diferenciaAdquirida :: (Gimnasta -> Float) -> Rutina -> Gimnasta -> Float
diferenciaAdquirida tipo rutina gimnasta = abs(tipo gimnasta - tipo (hacerRutina rutina gimnasta))

------------5
loHacenSaludable :: [Rutina] -> Gimnasta -> [Rutina]
loHacenSaludable rutinas gimnasta = filter (esSaludable gimnasta) rutinas    


esSaludable :: Gimnasta -> Rutina -> Bool
esSaludable gimnasta rutina = saludable (hacerRutina rutina gimnasta)

todosLosEjercicios ::   Rutina
todosLosEjercicios= UnaRutina "super man" 100 [(pesas 10),(colina 10),(montania 10),entrenamientoEnCinta,caminataEnCinta]