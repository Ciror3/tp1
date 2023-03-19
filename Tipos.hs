module Tipos where
type Datos = String
type Etiqueta = String
type Nombre = String

insertar :: Ord a => a -> [a] -> [a]
insertar cancion [] = [cancion] -- convierte a cancion en una lista asi compara
insertar cancion (lista:ys) | cancion < lista = cancion:lista:ys --En ys vas metiendo las canciones lista es tomada como cada elemento individual
                            | otherwise = lista:(insertar cancion ys)--elemento recursivo

            