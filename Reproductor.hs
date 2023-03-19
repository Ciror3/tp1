module Reproductor ( Reproductor, nuevoR, archivosR, listaParaR, temasR, playR, actualR, avanzarR, retrocederR,reiniciarR)where

import Tipos
import Tema
import Playlist
import FileSystem

data Reproductor = RP FileSystem Playlist deriving (Eq, Show)

nuevoR :: FileSystem -> Reproductor
nuevoR fs = RP fs (0,[]) 
-- Crea un nuevo reproductor a partir de un FileSystem y una nueva Playlist con su
-- lista de temas vac ́ıa.
archivosR :: Reproductor -> FileSystem
archivosR (RP fs _) =  fs
--listaParaR :: Etiqueta -> Reproductor ->[Tema]
--listasParaR eti (RP fs playlist) =  

temasR :: Reproductor ->[Tema]
temasR (RP _ ps) = actualP ps

-- playR :: Reproductor ->Etiqueta ->Reproductor
-- playR (RP fs ps) eti = 
-- actualR :: Reproductor ->Tema
-- avanzarR :: Reproductor ->Reproductor
-- retrocederR :: Reproductor ->Reproductor
-- reiniciarR :: Reproductor ->Reproductor