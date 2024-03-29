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
resetP (Play n temas) = Play 0 temas

-- TESTING --

testnuevaP =[nuevaP [] == Play 0 [],--True
             nuevaP [nuevoT "temas" "ahora"] == Play 0 [nuevoT "temas" "ahora"],--True
             nuevaP [nuevoT "temas" "ahora"] == Play 0 [nuevoT "temas" "ahoa"], --False
             nuevaP [nuevoT "temas" "ahora", nuevoT "como" "hace"] == Play 0 [nuevoT "temas" "ahora" , nuevoT "como" "hace"]]--True

testactualP = [actualP (Play 0 [nuevoT "temas" "ahora"]) == nuevoT "temas" "ahora",--True
               actualP (Play 1 [nuevoT "temas" "ahora"]) == nuevoT "temas" "ahora"]--error index to large

testskipP = [skipP (Play 0 [nuevoT "temas" "ahora"]) == Play 1 [nuevoT "temas" "ahora"], --True
             skipP (Play 2 [nuevoT "temas" "ahora"]) == Play 3 [nuevoT "temas" "ahora"], --True
             skipP (Play 2 [nuevoT "temas" "ahora"]) == Play 1 [nuevoT "temas" "ahora"]] --False

testbackP = [backP (Play 1 [nuevoT "temas" "ahora"]) == Play 0 [nuevoT "temas" "ahora"], --True
             backP (Play 2 [nuevoT "temas" "ahora"]) == Play 1 [nuevoT "temas" "ahora"], --True
             backP (Play 2 [nuevoT "temas" "ahora"]) == Play 3 [nuevoT "temas" "ahora"]] --False

testresetP = [resetP (Play 3 [nuevoT "temas" "ahora"]) == Play 0 [nuevoT "temas" "ahora"]]
