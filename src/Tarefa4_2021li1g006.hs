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
moveJogador (Jogo m j) movimento = Jogo m (alteraJogador j movimento m)

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) = correrMovimentos (moveJogador jogo h) t

-- Devolve o jogador na posição correta após efetuar o movimento
alteraJogador :: Jogador -> Movimento -> Mapa -> Jogador 
alteraJogador j AndarDireita m = andaDirJogador j m
alteraJogador j AndarEsquerda m = andaEsqJogador j m
-- alteraJogador j InterageCaixa m = interageCaixa j m
-- alteraJogador j@(Jogador c dir b) Trepar m | dir == Este = trepaDir j m
--                                            | otherwise = trepaEsq j m

-- NOTA: confirmar que nao ha peças vazias quando y é 0
-- NOTA2: adicionar caso quando é porta
andaDirJogador :: Jogador -> Mapa -> Jogador
andaDirJogador (Jogador (x,y) dir caixa) m 
             | getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa = Jogador (x,y) Este caixa
             | getPeca m (x+1) y == Vazio =
                    if getPeca m (x+1) (y-1) == Bloco || getPeca m (x+1) (y-1) == Caixa then Jogador (x+1,y) Este caixa
                    else andaDirJogador (Jogador (x,y-1) Este caixa) m


andaEsqJogador :: Jogador -> Mapa -> Jogador 
andaEsqJogador (Jogador (x,y) esq caixa) m 
            | getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa = Jogador (x,y) Oeste caixa
             | getPeca m (x-1) y == Vazio =
                    if getPeca m (x-1) (y-1) == Bloco || getPeca m (x-1) (y-1) == Caixa then Jogador (x-1,y) Oeste caixa
                    else andaEsqJogador (Jogador (x,y-1) Oeste caixa) m

-- trepa (fazer) -> TER EM CONSIDERAÇÃO A DIREÇÃO DO JOGADOR <-
--     trepaDir
--     trepaEsq

-- interageCaixa (fazer)

-- Devolve o tipo de peca duma dada posicao no mapa
getPeca :: Mapa -> Int -> Int -> Peca
getPeca [l] x 0 = getLinha l x
getPeca (h:t) x y = getPeca t x (y-1)

-- Axuliar para a getPeca que devolve a peca de uma linha de acordo com uma coordenada x
getLinha :: [Peca] -> Int -> Peca
getLinha [p] 0 = p
getLinha (h:t) x = getLinha t (x-1)