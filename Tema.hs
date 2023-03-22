module Tema (Tema, nuevoT ,nombreT, datosT, etiquetasT, agregarT, aplicaT )
    where

import Tipos

data Tema = Tem Nombre [Etiqueta] Datos deriving (Eq,Show,Ord)

nuevoT :: Nombre -> Datos -> Tema
nuevoT nombre = Tem nombre []
testnuevoT= [nuevoT "hola" "v" == Tem "hola" [] "v", --True
             nuevoT "hola" [] == Tem "hola" [] "",   --True 
             nuevoT "hola" [] == Tem "hola" [] []]   --True

nombreT :: Tema -> Nombre
nombreT (Tem nom _ _) = nom 
testnombreT = [nombreT (Tem "hola" [] "chau") == "hola", --True
               nombreT (Tem "hola" [] "chau") == "chau"] --False

datosT :: Tema -> Datos
datosT (Tem _ _ dat) = dat 
testdatosT = [datosT (Tem "hola" [] "chau") == "hola", --False
              datosT (Tem "hola" [] "chau") == "chau"] --True

etiquetasT :: Tema -> [ Etiqueta ]
etiquetasT (Tem _ eti _) = eti 
testetiquetasT = [etiquetasT (Tem "hola" ["etiqueta"] "chau") == ["etiqueta"], --True
                  etiquetasT (Tem "hola" [] "chau") == []]                     --True

agregarT :: Etiqueta ->Tema ->Tema
agregarT neweti (Tem nom eti dat) = Tem nom (eti++[neweti]) dat
testagregarT = [agregarT "eti2" (Tem "" ["eti1"] "") == Tem "" ["eti1","eti2"] "", --True 
                agregarT "eti2" (Tem "" ["eti1"] "") == Tem "" ["eti2","eti1"] ""] --False

aplicaT :: Etiqueta ->Tema ->Bool
aplicaT neweti (Tem nom eti dat) = [neweti] == eti
testaplicaT = [aplicaT "eti1" (Tem "" ["eti1"] "" ), --True 
               aplicaT "eti2" (Tem "" ["eti1"] "" )] --False