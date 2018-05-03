import Control.Monad
import Data.List (find, nub)
import Control.Applicative
import Data.Maybe
import Data.Monoid

data Persona = Persona { padres :: [Persona], hijos :: [Persona], pareja :: Maybe Persona, nombre :: String }

nuevo :: String -> Persona
nuevo nombre = Persona { padres = [], hijos = [], pareja = Nothing, nombre = nombre }
hugo = nuevo "Hugo"
juan = nuevo "Juan"
agus = nuevo "Agus"
moni = nuevo "Moni"
marcelo = nuevo "Marcelo"
silvana = nuevo "Silvana"
juli = nuevo "Juli"
lolo = nuevo "Lolo"
amanda = nuevo "Amanda"
sergio = nuevo "Sergio"
roxana = nuevo "Roxana"
rocio = nuevo "Rocio"
ezequiel = nuevo "Ezequiel"
marga = nuevo "Marga"
elena = nuevo "Elena"

actualizarRelaciones :: Persona -> Persona
actualizarRelaciones nodo = nuevoNodo
    where nuevoNodo = nodo { pareja = nuevaPareja, hijos = nuevosHijos, padres = nuevosPadres }
          nuevaPareja = actualizarRelaciones <$> (\pareja -> pareja { pareja = Just nuevoNodo }) <$> (pareja nodo)
          nuevosHijos = actualizarRelaciones <$> (\hijo -> hijo { padres = nuevoNodo : filter (/= nodo) (padres hijo) }) <$> (hijos nodo)
          nuevosPadres = actualizarRelaciones <$> (\padre -> padre { hijos = nuevoNodo : filter (/= nodo) (hijos padre) }) <$> (padres nodo)

agregarHijos :: [Persona] -> Persona -> Persona
agregarHijos nodosHijos nodoPadre = nuevoPadre
    where nuevoPadre = nodoPadre { hijos = nuevosHijos ++ hijos nodoPadre}
          nuevosHijos = fmap (agregarPadre nuevoPadre) nodosHijos
agregarHijo :: Persona -> Persona -> Persona
agregarHijo nodoHijo nodoPadre = actualizarRelaciones $ nuevoPadre
    where nuevoPadre = nodoPadre { hijos = nuevosHijos }
          nuevosHijos = fmap actualizarRelaciones $ nodoHijo { padres = nuevoPadre : padres nodoHijo } : fmap (\hijo -> hijo { padres = nuevoPadre : padres hijo}) (hijos nodoPadre)
agregarPadre :: Persona -> Persona -> Persona
agregarPadre nodoPadre nodoHijo = actualizarRelaciones $ head nuevosHijos
    where nuevoPadre = nodoPadre { hijos = nuevosHijos }
          nuevosHijos = fmap actualizarRelaciones $ nodoHijo { padres = nuevoPadre : padres nodoHijo } : fmap (\hijo -> hijo { padres = nuevoPadre : padres hijo}) (hijos nodoPadre)
casar :: Persona -> Persona -> Persona
casar nodoPareja nodoBase = actualizarRelaciones $ nodoBase { pareja = Just nuevaPareja, hijos = nuevosHijos }
    where nuevaPareja = nodoPareja { hijos = nuevosHijos }
          nuevosPadres = [nodoBase, nuevaPareja]
          nuevosHijos = fmap (\hijo -> hijo { padres = nuevosPadres ++ padres hijo}) (hijos nodoBase ++ hijos nodoPareja)

-- esto no siempre funciona, hay casos que no encuentra
encontrar :: String -> Persona -> Maybe Persona
encontrar n nodo = encontrarEntreHijos n (hijos nodo) <|>
                        encontrarEntrePadres n (padres nodo) <|>
                        encontrarEnPareja n nodo <|>
                        encontrarEntreHermanos n (hermanxs nodo)
encontrarEntreHermanos :: String -> [Persona] -> Maybe Persona
encontrarEntreHermanos _ [] = Nothing                        
encontrarEntreHermanos n nodos = find ((==n).nombre) nodos
encontrarEnPareja :: String -> Persona -> Maybe Persona
encontrarEnPareja n nodo = find ((==n).nombre) (pareja nodo)
encontrarEntreHijos :: String -> [Persona] -> Maybe Persona
encontrarEntreHijos _ [] = Nothing
encontrarEntreHijos n nodos = find ((==n).nombre) nodos <|> encontrarEntreHijos n (nodos >>= hijos)
encontrarEntrePadres :: String -> [Persona] -> Maybe Persona
encontrarEntrePadres _ [] = Nothing
encontrarEntrePadres n nodos = find ((==n).nombre) nodos <|> encontrarEntrePadres n (nodos >>= padres)

obtenerFamiliares :: (Persona -> [Persona]) -> Persona -> [Persona]
obtenerFamiliares f nodo = nub . filter (/= nodo) . f $ nodo

(>>>) :: [Persona] -> (Persona -> [Persona]) -> [Persona]
foo >>> f = nub $ foo >>= f

pareja' :: Persona -> [Persona]
pareja' = maybeToList . pareja

abuelxs :: Persona -> [Persona]
abuelxs = obtenerFamiliares $ padres >=> padres
tixAbuelxs :: Persona -> [Persona]
tixAbuelxs = obtenerFamiliares $ abuelxs >=> hermanxs
tixs :: Persona -> [Persona]
tixs = obtenerFamiliares $ padres >=> hermanxs
hermanxs :: Persona -> [Persona]
hermanxs = obtenerFamiliares $ padres >=> hijos
primxs :: Persona -> [Persona]
primxs = obtenerFamiliares $ tixs >=> hijos
sobrinxs :: Persona -> [Persona]
sobrinxs = obtenerFamiliares $ hermanxs >=> hijos
nietxs :: Persona -> [Persona]
nietxs = obtenerFamiliares $ hijos >=> hijos
suegrxs :: Persona -> [Persona]
suegrxs = obtenerFamiliares $ pareja' >=> padres
cuñadxs :: Persona -> [Persona]
cuñadxs = obtenerFamiliares $ (pareja' >=> hermanxs) <> (hermanxs >=> pareja')

sergio' = casar roxana . agregarHijo rocio . agregarHijo ezequiel $ sergio
marcelo' = casar silvana . agregarHijo lolo . agregarHijo juli $ marcelo
monica' = casar hugo . agregarHijo juan . agregarHijo agus $ moni
amanda' = agregarHijos [sergio', marcelo', monica'] amanda
elena' = agregarHijos [amanda', marga] elena

instance Show Persona where
    show = nombre

instance Eq Persona where
    a == b = nombre a == nombre b
