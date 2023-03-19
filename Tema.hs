module Tema (Tema, nuevoT ,nombreT, datosT, etiquetasT, agregarT, aplicaT )
    where

import Tipos

data Tema = Tem Nombre [Etiqueta] Datos deriving (Eq,Show,Ord)

nuevoT :: Nombre -> Datos -> Tema
nuevoT nombre = Tem nombre []

nombreT :: Tema -> Nombre
nombreT (Tem nom _ _) = nom 
 
datosT :: Tema -> Datos
datosT (Tem _ _ dat) = dat 

etiquetasT :: Tema -> [ Etiqueta ]
etiquetasT (Tem _ eti _) = eti 

agregarT :: Etiqueta ->Tema ->Tema
agregarT neweti (Tem nom eti dat) = Tem nom (eti++[neweti]) dat

aplicaT :: Etiqueta ->Tema ->Bool
aplicaT competi (Tem nom eti dat) = [competi] == eti
