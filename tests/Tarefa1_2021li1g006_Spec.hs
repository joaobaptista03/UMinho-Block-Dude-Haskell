module Tarefa1_2021li1g006_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g006
import Fixtures

-- Tarefa 1
testsT1 =
  test
    ["Tarefa 1 - Teste 1 - Vazio" ~: validaPotencialMapa [] ~=?  False
    , "Tarefa 1 - Teste 2 - 2 Peças" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 3" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 4" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 5" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 7" ~: validaPotencialMapa m1 ~=?  True
    , "Tarefa 1 - Teste 8 - 2 Portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 9 - 2 Portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (0,0))] ~=?  False
    , "Tarefa 1 - Teste 10" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Porta, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 11" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4))] ~=?  True
    , "Tarefa 1 - Teste 12" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,1)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4))] ~=?  False
    , "Tarefa 1 - Teste 13" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(6,4))] ~=?  False
    , "Tarefa 1 - Teste 14" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,5)),(Caixa,(3,4))] ~=?  False
    , "Tarefa 1 - Teste 15" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Bloco,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4)),(Caixa,(0,3))] ~=? True 
    ]
