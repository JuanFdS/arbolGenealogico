import Control.Monad
import Data.List (find, nub)
import Control.Applicative
import Data.Maybe
import Data.Monoid

data Arbol = Nodo { padres :: [Arbol], hijos :: [Arbol], pareja :: Maybe Arbol, nombre :: String }

nuevo nombre = Nodo { padres = [], hijos = [], pareja = Nothing, nombre = nombre }
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

modificarHijos :: ([Arbol] -> [Arbol]) -> Arbol -> Arbol
modificarHijos f nodo = nodo { hijos = f . hijos $ nodo }
modificarPadres :: ([Arbol] -> [Arbol]) -> Arbol -> Arbol
modificarPadres f nodo = nodo { padres = f . padres $ nodo }
actualizarHijos nodo = modificarHijos (fmap (\hijo -> hijo { padres = nodo : (filter (/= nodo) $ padres hijo) })) nodo
actualizarPadres nodo = modificarPadres (fmap (\padre -> padre { hijos = nodo : (filter (/= nodo) $ hijos padre) })) nodo

actualizarRelaciones nodo = nuevoNodo
    where nuevoNodo = nodo { pareja = nuevaPareja, hijos = nuevosHijos, padres = nuevosPadres }
          nuevaPareja = actualizarRelaciones <$> (\pareja -> pareja { pareja = Just nuevoNodo }) <$> (pareja nodo)
          nuevosHijos = actualizarRelaciones <$> (\hijo -> hijo { padres = nuevoNodo : filter (/= nodo) (padres hijo) }) <$> (hijos nodo)
          nuevosPadres = actualizarRelaciones <$> (\padre -> padre { hijos = nuevoNodo : filter (/= nodo) (hijos padre) }) <$> (padres nodo)

agregarHijos nodosHijos nodoPadre = nuevoPadre
    where nuevoPadre = nodoPadre { hijos = nuevosHijos ++ hijos nodoPadre}
          nuevosHijos = fmap (agregarPadre nuevoPadre) nodosHijos
agregarHijo :: Arbol -> Arbol -> Arbol
agregarHijo nodoHijo nodoPadre = actualizarRelaciones $ nuevoPadre
    where nuevoPadre = nodoPadre { hijos = nuevosHijos }
          nuevosHijos = fmap actualizarRelaciones $ nodoHijo { padres = nuevoPadre : padres nodoHijo } : fmap (\hijo -> hijo { padres = nuevoPadre : padres hijo}) (hijos nodoPadre)
agregarPadre nodoPadre nodoHijo = actualizarRelaciones $ head nuevosHijos
    where nuevoPadre = nodoPadre { hijos = nuevosHijos }
          nuevosHijos = fmap actualizarRelaciones $ nodoHijo { padres = nuevoPadre : padres nodoHijo } : fmap (\hijo -> hijo { padres = nuevoPadre : padres hijo}) (hijos nodoPadre)
casar nodoPareja nodoBase = actualizarRelaciones $ nodoBase { pareja = Just nuevaPareja, hijos = nuevosHijos }
    where nuevaPareja = nodoPareja { hijos = nuevosHijos }
          nuevosPadres = [nodoBase, nuevaPareja]
          nuevosHijos = fmap (\hijo -> hijo { padres = nuevosPadres ++ padres hijo}) (hijos nodoBase ++ hijos nodoPareja)

encontrar n nodo = encontrarEntreHijos n (hijos nodo) <|>
                        encontrarEntrePadres n (padres nodo) <|>
                        encontrarEnPareja n nodo <|>
                        encontrarEntreHermanos n (hermanxs nodo)

encontrarEntreHermanos _ [] = Nothing                        
encontrarEntreHermanos n nodos = find ((==n).nombre) nodos
encontrarEnPareja n nodo = find ((==n).nombre) (pareja nodo)
encontrarEntreHijos _ [] = Nothing
encontrarEntreHijos n nodos = find ((==n).nombre) nodos <|> encontrarEntreHijos n (nodos >>= hijos)
encontrarEntrePadres _ [] = Nothing
encontrarEntrePadres n nodos = find ((==n).nombre) nodos <|> encontrarEntrePadres n (nodos >>= padres)

obtenerFamiliares f nodo = nub . filter (/= nodo) . f $ nodo

foo >>> f = nub $ foo >>= f

pareja' = maybeToList . pareja

abuelxs = obtenerFamiliares $ padres >=> padres
tixAbuelxs = obtenerFamiliares $ abuelxs >=> hermanxs
tixs = obtenerFamiliares $ padres >=> hermanxs
hermanxs = obtenerFamiliares $ padres >=> hijos
primxs = obtenerFamiliares $ tixs >=> hijos
sobrinxs = obtenerFamiliares $ hermanxs >=> hijos
nietxs = obtenerFamiliares $ hijos >=> hijos
suegrxs = obtenerFamiliares $ pareja' >=> padres
cuñadxs = obtenerFamiliares $ (pareja' >=> hermanxs) <> (hermanxs >=> pareja')

sergio' = casar roxana . agregarHijo rocio . agregarHijo ezequiel $ sergio
marcelo' = casar silvana . agregarHijo lolo . agregarHijo juli $ marcelo
monica' = casar hugo . agregarHijo juan . agregarHijo agus $ moni
amanda' = agregarHijos [sergio', marcelo', monica'] amanda
elena' = agregarHijos [amanda', marga] elena

instance Show Arbol where
    show = nombre

instance Eq Arbol where
    a == b = nombre a == nombre b
