module FileSystem ( FileSystem, nuevoF, etiquetasF, temasF, agregarF)--, filtrarF )
where
import Tipos
import Tema
data FileSystem = FS [Etiqueta] [Tema] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []
--Crea un nuevo FileSystem con sus listas vac ́ıas.
etiquetasF :: FileSystem -> [ Etiqueta ]
etiquetasF (FS eti _) = eti
temasF :: FileSystem ->[ Tema ]
temasF (FS _ tem) = tem
agregarF :: Tema ->FileSystem ->FileSystem
agregarF tema (FS eti tem) = FS eti (tem ++ [tema])
-- --Agrega el tema y sus etiquetas de ser necesario.
-- filtrarF :: Etiqueta ->FileSystem ->[ Tema ]
-- filtrarF 