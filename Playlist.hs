module Playlist ( Playlist, nuevaP, actualP, skipP, backP, resetP )
where

import Tipos
import Tema

data Playlist = Play Int [ Tema ] deriving (Eq, Show)

--nuevoT nombre datos 
nuevaP :: [Tema] -> Playlist
nuevaP = Play 0 

actualP :: Playlist -> Tema
actualP (Play n tema) = tema !! n

skipP :: Playlist -> Playlist
skipP (Play n tema) |  n < 0 = error "Indice negativo"
                    | otherwise = Play (n+1) tema

backP :: Playlist -> Playlist
backP (Play n tema) | n < 1 = error "Indice negativo"
                    | otherwise = Play (n-1) tema

resetP :: Playlist -> Playlist
resetP (Play n temas) = Play n temas
