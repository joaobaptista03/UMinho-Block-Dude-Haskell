module Tarefa2_2021li1g006_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g006
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa 1" ~: pecas [(Bloco,(4,1)),(Bloco,(2,1)),(Bloco,(5,1)),(Bloco,(1,1)),(Caixa,(3,1))] ~=? [Bloco,Bloco,Bloco,Bloco,Caixa]
    , "Tarefa 2 - Teste Construir Mapa 2" ~: listaslinha 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] ~=? [(Porta,(0,0)),(Caixa,(2,1))]
    , "Tarefa 2 - Teste Construir Mapa 3" ~: ordPecasx [(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,2)),(Bloco,(1,2)),(Caixa,(3,2))] ~=? [(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(3,2)),(Bloco,(4,2)),(Bloco,(5,2))]
    , "Tarefa 2 - Teste Construir Mapa 4" ~: constroiMapa  [(Porta,(0,0)),(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,3)),(Bloco,(5,2)),(Bloco,(1,2)),(Bloco,(1,1)),(Caixa,(3,2))] ~=? [[Porta,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Bloco,Caixa,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]]
    ]