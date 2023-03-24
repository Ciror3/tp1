module FileSystem ( FileSystem, nuevoF, etiquetasF, temasF, agregarF, filtrarF )
where
import Tipos
import Tema
data FileSystem = FS [Etiqueta] [Tema] deriving (Eq, Show)

nuevoF :: FileSystem
nuevoF = FS [] []
--Crea un nuevo FileSystem con sus listas vacıas.

etiquetasF :: FileSystem -> [ Etiqueta ]
etiquetasF (FS eti _) = eti                  

temasF :: FileSystem -> [ Tema ]
temasF (FS _ tem) = tem

agregarF :: Tema ->FileSystem ->FileSystem
agregarF newtema (FS eti tem) = FS eti (insertar newtema tem)
-- --Agrega el tema y sus etiquetas de ser necesario.

filtrarF :: Etiqueta ->FileSystem ->[ Tema ]
filtrarF neweti (FS eti tem) = [x| x <- tem, aplicaT neweti x]




-- TESTING --

testnuevoF = [nuevoF == FS [] []] --True
testetiquetasF = [etiquetasF (FS ["eti1","eti2"] []) == ["eti1","eti2"], --True
                  etiquetasF (FS ["eti1","eti2"] []) == ["eti1"]]        --False
testtemasF = [temasF (FS [] [nuevoT "Back to black" "Amy winehouse"]) == [nuevoT "Back to black" "Amy winehouse"], --True
              temasF (FS [] [nuevoT "Back to black" "Amy winehouse", nuevoT "h" "o"]) == [nuevoT "Back to black" "Amy winehouse", nuevoT "h" "o"],--True
              temasF (FS [] [nuevoT "Back to black" "Amy winehouse", nuevoT "h" "o"]) == [nuevoT "Back to black" "Amy winehouse", nuevoT "h" ""]]--False
testagregarF = [agregarF (nuevoT "Boca yo te amo" "mc caco") (FS [] [nuevoT "c" "w"]) == FS [] [nuevoT "c" "w" , nuevoT "Boca yo te amo" "mc caco"],--False
                agregarF (nuevoT "Boca yo te amo" "mc caco") (FS [] [nuevoT "c" "w"]) == FS [] [nuevoT "Boca yo te amo" "mc caco",nuevoT "c" "w" ]] --True
testfiltrarF = [filtrarF "Rock" (FS [] [agregarT "Rock" (nuevoT "h" "l")]) == [agregarT "Rock" (nuevoT "h" "l")], --True
                filtrarF "Rock" (FS [] [agregarT "Rock" (nuevoT "h" "l"), agregarT "Trap" (nuevoT "no vendo trap" "Duki")]) == [agregarT "Rock" (nuevoT "h" "l")],-- True
                filtrarF "Rock" (FS [] [agregarT "Rock" (nuevoT "h" "l"), agregarT "Trap" (nuevoT "no vendo trap" "Duki")]) == [agregarT "Trap" (nuevoT "no vendo trap" "Duki")],--False
                filtrarF "Rock" (FS [] [agregarT "Rock" (nuevoT "h" "l"), agregarT "Rock"(agregarT "Trap" (nuevoT "no vendo trap" "Duki"))]) == [agregarT "Rock" (nuevoT "h" "l"),agregarT "Rock"(agregarT "Trap" (nuevoT "no vendo trap" "Duki"))]]--True