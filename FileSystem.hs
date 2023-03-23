module FileSystem ( FileSystem, nuevoF, etiquetasF, temasF, agregarF, filtrarF )
where
import Tipos
import Tema
data FileSystem = FS [Etiqueta] [Tema] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []
--Crea un nuevo FileSystem con sus listas vacıas.
testnuevoF = [nuevoF == FS [] []] --True

etiquetasF :: FileSystem -> [ Etiqueta ]
etiquetasF (FS eti _) = eti
testetiquetasF = [etiquetasF (FS ["eti1","eti2"] []) == ["eti1","eti2"], --True
                  etiquetasF (FS ["eti1","eti2"] []) == ["eti1"]]        --False
                  

temasF :: FileSystem -> [ Tema ]
temasF (FS _ tem) = tem
testtemasF = []

agregarF :: Tema -> Etiqueta ->FileSystem ->FileSystem
agregarF newtema neweti (FS eti tem) = FS (eti++[neweti]) (insertar newtema tem)
-- --Agrega el tema y sus etiquetas de ser necesario.
testagregarF = []

filtrarF :: Etiqueta ->FileSystem ->[ Tema ]
filtrarF neweti (FS eti tem) = [x | x <- tem, aplicaT neweti x]
testfiltrarF = []