module Tipos where
type Datos = String
type Etiqueta = String
type Nombre = String

insertar :: Ord a => a -> [a] -> [a]
insertar cancion [] = [cancion] 
insertar cancion (lista:ys) | cancion < lista = cancion:lista:ys 
                            | otherwise = lista: insertar cancion ys


-- TESTING --
test = [insertar "hola"[] == ["hola"],
       insertar "hola"["a"] == ["a","hola"],
       insertar "hola"["a", "z"] == ["a","hola","z"],
       insertar "hola"["a", "z"] == ["a","z","hola"]]
            