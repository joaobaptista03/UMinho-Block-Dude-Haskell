module Tarefa1_2021li1g006_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g006
import Fixtures

-- Tarefa 1
testsT1 =
  test
    ["Tarefa 1 - Teste 1 - Vazio" ~: validaPotencialMapa [] ~=?  True
    , "Tarefa 1 - Teste 2 - 2 Peças" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1))] ~=?  True
    , "Tarefa 1 - Teste 3" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 4" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 5" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 6" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3)), (Bloco, (0,4))] ~=?  True
    , "Tarefa 1 - Teste 7" ~: validaPotencialMapa m1 ~=?  True
    , "Tarefa 1 - Teste 8 - 2 Portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (0,1))] ~=?  False
    , "Tarefa 1 - Teste 9 - 2 Portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (0,0))] ~=?  False
    , "Tarefa 1 - Teste 10" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Porta, (0,1))] ~=?  False
    ]