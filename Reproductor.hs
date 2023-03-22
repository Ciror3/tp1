module Reproductor ( Reproductor, nuevoR, archivosR, temasR, actualR, avanzarR, retrocederR,reiniciarR)
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
listasparaR :: Etiqueta -> Reproductor ->[Tema] --va a las etiquetas de filesystem y devuelve la lista de temas
listasparaR eti (RP fs playlist) = filtrarF eti fs 
--a playlist le cambio el n y chequeo aplicat eti (actualP(skipP(playlist)))
temasR :: Reproductor ->[Tema]
temasR (RP fs _) = temasF fs

playR :: Reproductor ->Etiqueta ->Reproductor -- usa listasparaR
playR (RP fs ps) neweti = RP fs (nuevaP(filtrarF neweti fs))

actualR :: Reproductor ->Tema
actualR (RP _ ps) = actualP ps
avanzarR :: Reproductor ->Reproductor
avanzarR (RP fs ps) = RP fs (skipP ps)
retrocederR :: Reproductor ->Reproductor
retrocederR (RP fs ps) = RP fs (backP ps)
reiniciarR :: Reproductor -> Reproductor
reiniciarR (RP fs ps) = RP fs (resetP ps)