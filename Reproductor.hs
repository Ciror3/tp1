module Reproductor ( Reproductor, nuevoR, archivosR, listasParaR, temasR, actualR, avanzarR, retrocederR,reiniciarR)
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
listasParaR eti (RP fs playlist) = filtrarF eti fs 

temasR :: Reproductor ->[Tema]
temasR (RP fs _) = temasF fs

playR :: Reproductor ->Etiqueta ->Reproductor 
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

testlistasParaR =[listasParaR]

testtemasR = [temasR (RP (agregarF (nuevoT "l" "a")  nuevoF) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "l" "a")  nuevoF),--True
              temasR (RP (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)), --True
              temasR (RP (agregarF (nuevoT "c" "m") (agregarF (nuevoT "l" "a")  nuevoF)) (nuevaP[nuevoT "" ""])) == temasF (agregarF (nuevoT "c" "m") (agregarF (nuevoT "" "a")  nuevoF))]--False

testplayR = [playR]

testactualR = [actualR (RP nuevoF (skipP(nuevaP [nuevoT "h" "m", nuevoT "earfquake" "Tyler"]))) == nuevoT "earfquake" "Tyler",--True
               actualR (RP nuevoF (skipP(nuevaP [nuevoT "h" "m", nuevoT "earfquake" "Tyler"]))) == nuevoT "h" "m"]--False

