{- |
Module      : Tarefa4_2021li1g006
Description : Movimentação do personagem
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g006 where

import LI12122
import GHC.Stack.Types (CallStack(FreezeCallStack))

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo movimento = undefined

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo movimentos = undefined

-- A função faz com que o jogador faça um movimento

moveEnabled :: a -> Bool
moveEnabled _ = False

move :: Coordenadas -> Movimento -> (index,index) 
move (x,y) AndaDireita = (x+1,y)
move (x,y) AndaEsquerda = (x-1,y)


moverp :: Jogador -> Movimento -> (index,index) -> Bool 
moverp _ _ [] = False