module Library where
import PdePreludat


data Turista = UnTurista {
    nivelCansancio :: Number,
    nivelStress :: Number,
    viajaSolo :: Bool,
    idiomas:: [String]
}

--Parte 1

type Excursion = Turista -> Turista

--Excursion1
irALaPlaya :: Excursion
irALaPlaya turista
    |viajaSolo turista = turista {nivelCansancio = nivelCansancio turista - 5}
    |otherwise = turista {nivelDeStress = nivelStress turista - 1}

--Excursion2

reducirSegunCantLetras :: Number -> String -> Number
reducirSegunCantLetras x elemento = x - length (elemento)

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento turista = turista{nivelStress = reducirSegunCantLetras (nivelStress turista) elemento}  

--Excursion3

agregarIdioma ::  String -> [String] -> [String]
agregarIdioma nuevoIdioma idioma= idioma ++ [nuevoIdioma]

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idioma turista = turista {idiomas = agregarIdioma idioma (idioma turista), viajaSolo = False}

--Excursion4

intensidadCaminata :: Number -> Number
intensidadCaminata minutos = div minutos 4

caminarCiertosMinutos :: Number -> Excursion
caminarCiertosMinutos tiempo turista = turista {nivelCansancio = intensidadCaminata tiempo, nivelStress = intensidadCaminata tiempo}

----Excursion5

data Marea = Fuerte | Moderada| Tranquila

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista 
    |marea == Fuerte = mareaFuerte turista
    |marea == Moderada = turista
    |marea == Tranquila = mareaTranquila turista

mareaFuerte :: Excursion
mareaFuerte turista = turista {nivelCansancio = nivelCansancio + 10, nivelStress = nivelStress turista + 6}

mareaTranquila :: Excursion
mareaTranquila = salirAHablarIdioma "aleman" . apreciarPaisaje "mar" . caminarCiertosMinutos 10

--Parte 2

excursiones :: Excursion
excursiones = [irALaPlaya, apreciarPaisaje "montaña", salirAHablarIdioma "aleman", caminarCiertosMinutos 20, paseoEnBarco Moderada]

ana :: Turista
ana = UnTurista {nivelCanasancio = 0, nivelStress = 21, viajaSolo = False, idiomas = ["español"]}

beto :: Turista
ana = UnTurista {nivelCanasancio = 15, nivelStress = 15, viajaSolo = True, idiomas = ["aleman"]}

cathi :: Turista
cathi = UnTurista {nivelCanasancio = 15, nivelStress = 15, viajaSolo = True, idiomas = ["aleman", "catalan"]}

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = multiplicarStress 0.9 . excursion

multiplicarStress :: Number -> Turista -> Turista
multiplicar x turista = turista {nivelStress = nivelStress turista * x}


-- Parte 3

--Funcion 1

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaSegunExcursion :: (a->Number) -> Turista -> Excursion -> Turista
deltaSegunExcursion indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

--Funcion 2

esEducativa ::  Turista -> Excursion -> Bool
esEducativa = (> 0) . diferenciaLongitudes

--aplicar deltaSegunExcursion
diferenciaLongitudes :: Turista -> Excursion -> Number
diferenciaLongitudes excursion = (length . idioma .hacerExcursion excursion) turista - (length (idiomas turista
))

--Ver interpretacion
excursionesDesestresantes :: Turista -> Excursion -> Bool
excursionesDesestresantes turista excursion = (>= 3) . deltaSegunExcursion nivelStress turista excursion

-- Parte 4

--Funcion 1

type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminarCiertosMinutos 20, apreciarPaisaje "cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarIdioma "melmacquiano"]

tourLadoB :: Excursion -> Tour
tourLadoB excursion = [paseoEnBarco Tranquila, excursion, caminarCiertosMinutos 120]

tourIslaVecina :: Marea -> Tour
tourIslaVecina marea excursion
    |marea == Fuerte = [paseoEnBarco Fuerte, apreciarPaisaje "lago", paseoEnBarco Fuerte]
    |marea == Moderada = [paseoEnBarco Moderada, irALaPlaya, paseoEnBarco Moderada]
    |marea == Tranquila = [paseoEnBarco Tranquila, irALaPlaya, paseoEnBarco Tranquila]

aumentoStressExcursion :: Turista -> Tour -> Turista
aumentoStressExcursion turista tour  = multiplicarStress(length tour) tourista

aplicarExcursion :: Turista ->Excursion -> Turista
aplicarExcursion turista excursion = excursion turista

hacerTour :: Tour -> Turista ->Excursion -> Turista
hacerTour tour turista excursion = foldl (aplicarExcursion excursion) (aumentoStressExcursion turista tour) tour


--Funcion 2

turistaAcompaniado :: Turista -> Excursion -> Bool
turistaAcompaniado turista = not (viajaSolo (excursion turista))


--Es un grupo de tours!!
condicionesEsConvincente :: Turista ->Excursion -> Bool
condicionesEsConvincente turista excursion = (turistaAcompaniado turista excursion) && (excursionesDesestresantes turista excursion)

listaEsConvincente :: Tour -> Turista ->Excursion -> Tour
listaEsConvincente tour turista excursion = filter (condicionesEsConvincente turista) tour

esConvincente :: Tour -> Turista ->Excursion -> Bool
esConvincente = not . null . listaEsConvincente 

-- Funcion 3

perdidaStress :: Tour -> Turista ->Excursion -> Number
perdidaStress  tour turista excursion = deltaSegun stress (hacerTour tour turista excursion) turista

perdidaCansancio:: Tour -> Turista ->Excursion -> Number
perdidaCansancio tour turista excursion = deltaSegun nivelCanasancio (hacerTour tour turista excursion) turista

espiritualidadTurista :: Tour -> Turista ->Excursion -> Number
espiritualidadTurista tour turista excursion =  (perdidaCansancio tour turista excursion) + (perdidaStress tour turista excursion)
--Ver que solo se sumen los convencidos
obtenerEfectividad :: Tour -> [Turista] ->Excursion -> [Number]
obtenerEfectividad tour turistas excursion = map (espiritualidadTurista tour excursion) turistas

efectividadTour :: Tour -> [Turista] ->Excursion -> Number
efectividadTour tour turistas excursion = sum (obtenerEfectividad tour turistas excursion)  

turistasConvencidos ::  [Turista] -> Tour -> [Turista] 
turistasConvencidos turistas tour = filter (esConvincente tour) turistas

--Parte 5

playasInfinitas :: Tour
playasInfinitas = irALaPlaya : playasInfinitas

{-
¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.

 Podemos asegurarnos que el tour no será convincente para ninguno de los dos ya que en ambos casos daría falso por que irALaPlaya no es desestresante. Por ende, sabiendo que Haskell opera con lazy evaluation, si encuentra un resultado falso frena la ejecución. Es decir, no espera a terminar recorrer toda la lista. 

¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

No, ya que se continuaria aplicando el tour infinitamente y, por ende, nunca terminaría de sumar la espirtualidad de cada turista. 
-}