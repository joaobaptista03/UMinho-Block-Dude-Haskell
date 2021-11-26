module Tarefa2_2021li1g006_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g006
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa 1 (Inválido)" ~: constroiMapa [(Bloco,(1,2)),(Bloco,(2,3)),(Bloco,(3,3))] ~=? []
    , "Tarefa 2 - Teste Construir Mapa 2" ~: constroiMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4)),(Caixa,(0,3))] ~=? [[Porta,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Caixa,Vazio,Vazio,Vazio],[Bloco,Caixa,Caixa,Caixa],[Bloco,Bloco,Bloco,Bloco]]
    , "Tarefa 2 - Teste Construir Mapa 3 (Inválido)" ~: constroiMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,5)),(Caixa,(3,4))] ~=? []
    , "Tarefa 2 - Teste Construir Mapa 4" ~: constroiMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4))] ~=? [[Porta,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Caixa,Caixa,Caixa,Caixa],[Bloco,Bloco,Bloco,Bloco]]
    , "Tarefa 2 - Teste Construir Mapa 4" ~: constroiMapa m1 ~=? m1r
    ]