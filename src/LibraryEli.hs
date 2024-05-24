module LibraryEli where
import PdePreludat

-- ------------------------ Dominio ---------------------------
data Turista = UnTurista{
    nivelCansancio :: Cansancio,
    nivelStress :: Stress,
    idiomas :: [Idioma],
    solo :: Bool,
    espiritualidad :: Espiritualidad
}

data Marea = Fuerte | Moderada | Tranquila 

-- ------------------------ Definición de Tipos ---------------------
type Cansancio = Number
type Stress = Number
type Idioma = String
type Espiritualidad -> Number
type Excursion = Turista -> Turista
type Indice = (Turista -> Number)
type Tour = [Excursion]

-- ------------------------ Funciones ------------------------------
-- --------------- Parte 1
-- Funcion 1

irALaPlaya :: Excursion
irALaPlaya turista = cambiosPorPlaya turista

cambiosPorPlaya :: Excursion
cambiosPorPlaya turista
    | solo turista == True = turista{
        nivelCansancio = cambiarTurista turista nivelCansancio (-5)
        }
    | otherwise = turista{nivelStress = cambiarTurista turista nivelStress (-1)}

cambiarTurista :: Turista -> (Turista -> Number) -> Number -> Number
cambiarTurista turista caracteristica cantidad = caracteristica turista + cantidad

-- Funcion 2
apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemPaisaje turista = cambiosPorPaisaje elemPaisaje turista

cambiosPorPaisaje :: String -> Excursion
cambiosPorPaisaje elemPaisaje turista = turista{
    nivelStress = cambiarTurista turista nivelStress (-length elemPaisaje)
    }

-- Funcion 3
hablarIdiomaEspecifico :: Idioma -> Excursion
hablarIdiomaEspecifico idioma turista = cambiosPorIdioma idioma turista

cambiosPorIdioma :: Idioma -> Excursion
cambiosPorIdioma idioma turista = turista{ 
    idiomas = idiomas turista ++ [idioma], 
    solo = False
    }

-- Funcion 4
caminar :: Number -> Excursion
caminar cantMinutos turista = cambiosPorCaminar cantMinutos turista

cambiosPorCaminar :: Number -> Excursion
cambiosPorCaminar cantMinutos turista = turista{
    cansancio = cambiarTurista turista nivelCansancio (nivelIntensidad cantMinutos)
    nivelStress = cambiarTurista turista nivelStress (-nivelIntensidad cantMinutos)
    }

nivelIntensidad :: Number -> Number
nivelIntensidad cantMinutos = div cantMinutos 4

-- Funcion 5
paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista
    | marea == Fuerte = cambiosMareaFuerte turista
    | marea == Moderada = turista
    | marea == Tranquila = cambiosMareaTranquila turista

cambiosMareaFuerte :: Excursion
cambiosMareaFuerte turista = turista{
    nivelCansancio = cambiarTurista turista nivelCansancio 10,
    nivelStress = cambiarTurista turista nivelStress 6
}

cambiosMareaTranquila :: Excursion
cambiosMareaTranquila = hablarIdiomaEspecifico "Aleman" . apreciarPaisaje "Mar" . caminar 10

-- ---------------- Modelaje de Turistas -----------------------

Ana :: Turista
Ana = unTurista 0 21 ["Espanniol"] False 

Beto :: Turista
Beto = unTurista 15 15 ["Aleman"] True

Cathi :: Turista
Cathi = unTurista 15 15 ["Aleman", "Catalan"] True 

-- --------------- Parte 2
-- Funcion 1
hacerExcursion :: Turista -> Excursion -> Turista
hacerExcursion excursion = reducirStress . excursion 

reducirStress :: Turista -> Turista
reducirStress turista = turista {
    nivelStress = nivelStress turista * 0.9
}

-- Funcion 2
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaSegunExcursion :: Indice -> Turista -> Excursion -> Number
deltaSegunExcursion indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

-- Funcion 3
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = deltaSegunExcursion idiomas turista excursion > 0

-- Funcion 4
excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes excursiones turista = filter ((<=3) . deltaSegunExcursion nivelStress turista) excursiones

-- --------------------------- Modelaje de Tours -------------------------
tourCompleto :: Tour
tourCompleto = [caminar 20, apreciarPaisaje "Cascada", caminar 40, irALaPlaya, hablarIdiomaEspecifico "melmacquiano"]

tourLadoB :: Excursion -> Tour
tourLadoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

tourIslaVecina :: Marea -> Tour
tourIslaVecina marea 
    | marea == Fuerte = [paseoEnBarco marea, apreciarPaisaje "Lago", paseoEnBarco marea]
    | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

tours :: [Tour]
ctoTours = [tourCompleto, tourLadoB irALaPlaya, tourIslaVecina Tranquila]  

-- --------------- Parte 3
-- Funcion 1
realizarTour :: Tour -> Excursion
realizarTour = realizarExcursiones . aumentoEstres

aumentoEstres :: Tour -> Turista -> Turista
aumentoEstres tour turista = turista{
    nivelStress = cambiarTurista turista nivelStress (length tour)
    }

realizarExcursiones :: Tour -> Excursion
realizarExcursiones tour turista = foldl (hacerExcursion turista) turista tour

-- Funcion 2
esConvincente :: [Tour] -> Turista -> Bool
esConvincente tours turista = tieneExcurDeses tours turista && noDejaTuristaSolo (excursionesDesestresantes tours turistas) turista

tieneExcurDeses :: [Tour] -> Turista -> Bool
tieneExcurDeses tours turista = any (not . null . flip excursionesDesestresantes turista) tours

noDejaTuristaSolo :: [Excursion] -> Turista -> Bool
noDejaTuristaSolo excursiones turista = not. solo . (flip hacerExcursion excursion) turista 

-- Funcion 3
esEfectivo :: [Turista] -> Tour -> Bool
esEfectivo turistas tour = sum (listaEspiritualidad) turistas tour

listaEspiritualidad :: [Turista] -> Tour -> [Number]
listaEspiritualidad turistas tour = map espiritualidad (modificaTuristasEspirit turistas tour)

modificaTuristasEspirit :: [Turistas] -> Tour -> [Turistas]
modificaTuristasEspirit turistas tour = map (aumentaEspirit tour) (turistasConvencidos turistas tour)

turistasConvencidos :: [Turista] -> Tour -> [Turista]
turistasConvencidos turistas tour = filter (esConvincente [tour]) turistas

aumentaEspirit :: Tour -> Excursion
aumentaEspirit tour turista = turista[
    nivelCansancio = nivelCansancio (realizarTour tour turista),
    nivelStress = nivelStress (realizarTour tour turista),
    espiritualidad = medirEspirit turista tour 
]

medirEspirit :: Turista -> Tour -> Number
medirEspirit turista tour = (- medirPropPorTour nivelStress turista tour) + (medirPropPorTour nivelCansancio turista tour)

medirPropPorTour :: Indice -> Turista -> Tour -> Number
medirPropPorTour indice turista tour = fold (deltaSegunExcursion indice) turista tour


-- -------------------- Parte Teorica -------------------------
tourPlayero :: Tour
tourPlayero = irALaPlaya : tourPlayero

{-

¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.

No se podría saber. Aunque el primer componente de la función utiliza un any,
haciendo posible que se evalue incluso si es infinita por el motor de
lazy evaluation de Haskell (al encontrar una verdad ya pararía de analizar), 
el segundo componente obliga a la funcion a conocer todas las excursiones 
desestresantes, lo cuál es imposible para un tour de excursiones infinitas. 

¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

No, porque utiliza la funcion esConvincente, que ya delimitamos no podria evaluarse. 


-}