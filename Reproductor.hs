module Reproductor ( Reproductor, nuevoR, archivosR, temasR)--, playR, actualR, avanzarR, retrocederR,reiniciarR)
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
--listaParaR :: Etiqueta -> Reproductor ->[Tema] va a las etiquetas de playlist
--listasParaR eti (RP fs playlist) =   

temasR :: Reproductor ->[Tema]
temasR (RP fs _) = temasF fs

-- playR :: Reproductor ->Etiqueta ->Reproductor va a las etiquetas de fylesistem
-- playR (RP fs ps) eti = if 
actualR :: Reproductor ->Tema
actualR (RP _ ps) = actualP ps
avanzarR :: Reproductor ->Reproductor
avanzarR (RP fs ps) = RP fs (skipP ps)
retrocederR :: Reproductor ->Reproductor
retrocederR (RP fs ps) = RP fs (backP ps)
reiniciarR :: Reproductor -> Reproductor
reiniciarR (RP fs ps) = RP fs (resetP ps)