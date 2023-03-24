module Tipos where
type Datos = String
type Etiqueta = String
type Nombre = String

insertar :: Ord a => a -> [a] -> [a]
insertar cancion [] = [cancion] 
insertar cancion (lista:ys) | cancion < lista = cancion:lista:ys 
                            | otherwise = lista: insertar cancion ys


-- TESTING --
test = [insertar "hola"[] == ["hola"], --TRUE
       insertar "hola"["a"] == ["a","hola"], --TRUE
       insertar "hola"["a", "z"] == ["a","hola","z"], --TRUE
       insertar "hola"["a", "z"] == ["a","z","hola"]] --FALSE
            