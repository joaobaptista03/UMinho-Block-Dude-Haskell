module Tarefa3_2021li1g006_Spec where
  
import LI12122
import Test.HUnit
import Tarefa3_2021li1g006
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX\n" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX\n" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo (3)" ~: "  X\n<  \nXXX\n" ~=?  show (Jogo [[Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco]] (Jogador (0, 1) Oeste False))
    , "Tarefa 3 - Teste Imprime Jogo (4)" ~: " >X\n X \nXXX\n" ~=?  show (Jogo [[Vazio,Vazio,Bloco],[Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco]] (Jogador (1, 0) Este False))
    , "Tarefa 3 - Teste Imprime Jogo (5)" ~: "C>X\n X \nXXX\n" ~=?  show (Jogo [[Caixa,Bloco,Bloco],[Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco]] (Jogador (1, 0) Este False))
    , "Tarefa 3 - Teste Imprime Jogo (6)" ~: "CX>\nCXX\nXXX\n" ~=?  show (Jogo [[Caixa,Bloco,Bloco],[Caixa,Bloco,Bloco],[Bloco,Bloco,Bloco]] (Jogador (2, 0) Este False))
    ]