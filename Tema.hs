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
aplicaT neweti (Tem nom eti dat) = elem neweti eti


-- TESTING --


testnuevoT= [nuevoT "hola" "v" == Tem "hola" [] "v", --True
             nuevoT "hola" [] == Tem "hola" [] "",   --True 
             nuevoT "hola" [] == Tem "hola" [] []]   --True

testdatosT = [datosT (Tem "hola" [] "chau") == "hola", --False
              datosT (Tem "hola" [] "chau") == "chau"] --True

testetiquetasT = [etiquetasT (Tem "hola" ["etiqueta"] "chau") == ["etiqueta"], --True
                  etiquetasT (Tem "hola" [] "chau") == []]                     --True

testagregarT = [agregarT "eti2" (Tem "" ["eti1"] "") == Tem "" ["eti1","eti2"] "", --True 
                agregarT "eti2" (Tem "" ["eti1"] "") == Tem "" ["eti2","eti1"] ""] --False

testaplicaT = [aplicaT "eti1" (Tem "" ["eti1"] "" ), --True 
               aplicaT "eti2" (Tem "" ["eti1","eti2"] "" ),--True
               aplicaT "eti3" (Tem "" ["eti1","eti2"] "" )] --False