module Reproductor ( Reproductor, nuevoR, archivosR, listasParaR, temasR, actualR, avanzarR, retrocederR, reiniciarR)
where

import Tipos
import Tema
import Playlist
import FileSystem

data Reproductor = RP FileSystem Playlist deriving (Eq, Show)

nuevoR :: FileSystem -> Reproductor
nuevoR fs = RP fs (nuevaP [nuevoT "" ""])
-- Crea un nuevo reproductor a partir de un FileSystem y una nueva Playlist con su
-- lista de temas vacıa.

archivosR :: Reproductor -> FileSystem
archivosR (RP fs _) =  fs

listasParaR :: Etiqueta -> Reproductor ->[Tema]
listasParaR neweti (RP fs playlist) = filtrarF neweti fs 

temasR :: Reproductor ->[Tema]
temasR (RP fs _) = temasF fs

playR :: Reproductor -> Etiqueta ->Reproductor 
playR (RP fs ps) neweti = RP fs (nuevaP(filtrarF neweti fs))

actualR :: Reproductor ->Tema
actualR (RP _ ps) = actualP ps

avanzarR :: Reproductor ->Reproductor
avanzarR (RP fs ps) = RP fs (skipP ps)

retrocederR :: Reproductor ->Reproductor
retrocederR (RP fs ps) = RP fs (backP ps)

reiniciarR :: Reproductor -> Reproductor
reiniciarR (RP fs ps) = RP fs (resetP ps)


-- TESTING --


testnuevoR = [nuevoR nuevoF == RP nuevoF (nuevaP [nuevoT "" ""])] --True

testarchivosR = [archivosR (RP (agregarF (nuevoT "l" "a")  nuevoF) (nuevaP[nuevoT "" ""])) == agregarF (nuevoT "l" "a") nuevoF]--True

testlistasParaR = [listasParaR "Rock" (RP (agregarF (agregarT "Rock"(nuevoT "l" "a")) nuevoF) (nuevaP[nuevoT "" ""])) == [agregarT "Rock"(nuevoT "l" "a")],--True
                   listasParaR "Rock" (RP (agregarF (agregarT "Rock"(nuevoT "m" "o")) (agregarF (agregarT "Rock"(nuevoT "l" "a")) nuevoF)) (nuevaP[nuevoT "" ""])) == [agregarT "Rock"(nuevoT "l" "a"), agregarT "Rock"(nuevoT "m" "o")],--True
                   listasParaR "Rock" (RP (agregarF (agregarT "Rap"(agregarT "Rock"(nuevoT "m" "o"))) (agregarF (agregarT "Rock"(nuevoT "l" "a")) nuevoF)) (nuevaP[nuevoT "" ""])) == [agregarT "Rock"(nuevoT "l" "a"), agregarT "Rap"(agregarT "Rock"(nuevoT "m" "o"))]]--True

testtemasR = [temasR (RP (agregarF (nuevoT "l" "a")  nuevoF) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "l" "a")  nuevoF),--True
              temasR (RP (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)), --True
              temasR (RP (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "c" "m") (agregarF (nuevoT "" "a")  nuevoF))]--False

testplayR = [playR (RP (agregarF (agregarT "Rock"(nuevoT "l" "a")) nuevoF) (nuevaP[nuevoT "" ""])) "Rock" == RP (agregarF (agregarT "Rock"(nuevoT "l" "a")) nuevoF) (nuevaP[agregarT "Rock"(nuevoT "l" "a")])]--True

testactualR = [actualR (RP nuevoF (skipP(nuevaP [nuevoT "h" "m", nuevoT "earfquake" "Tyler"]))) == nuevoT "earfquake" "Tyler",--True
               actualR (RP nuevoF (skipP(nuevaP [nuevoT "h" "m", nuevoT "earfquake" "Tyler"]))) == nuevoT "h" "m"]--False

testavanzarR = [avanzarR (RP nuevoF (nuevaP [nuevoT "h" "l"])) == RP nuevoF (skipP(nuevaP [nuevoT "h" "l"])),--True
                avanzarR (RP nuevoF (nuevaP [nuevoT "h" "l"])) == RP nuevoF (backP(nuevaP [nuevoT "h" "l"]))]--Error indice negativo

testretrocederR = [retrocederR (RP nuevoF (skipP(nuevaP [nuevoT "h" "l"]))) == RP nuevoF (nuevaP [nuevoT "h" "l"]),--True
                   retrocederR (RP nuevoF (nuevaP [nuevoT "h" "l"])) == RP nuevoF (nuevaP [nuevoT "h" "l"])]--Error indice negativo

testreiniciarR = [reiniciarR (RP nuevoF (skipP(nuevaP [nuevoT "h" "l"]))) == RP nuevoF (nuevaP [nuevoT "h" "l"]),--True
                  reiniciarR (RP nuevoF (backP(nuevaP [nuevoT "h" "l"]))) == RP nuevoF (nuevaP [nuevoT "h" "l"])]--error indice negativo