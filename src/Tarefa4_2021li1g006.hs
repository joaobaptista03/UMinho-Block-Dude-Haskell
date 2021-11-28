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
import Language.Haskell.TH (javaScript)

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m j) AndarDireita = Jogo m (andaDirJogador j m)
moveJogador (Jogo m j) AndarEsquerda = Jogo m (andaEsqJogador j m)
moveJogador (Jogo m j) Trepar = Jogo m (trepa j m)
moveJogador j InterageCaixa = interageCaixa j


correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) = correrMovimentos (moveJogador jogo h) t


trepa :: Jogador -> Mapa -> Jogador 
trepa j@(Jogador (x,y) dir caixa) m 
                     | not caixa && dir == Este && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && (getPeca m (x+1) (y+1) == Vazio || getPeca m (x+1) (y+1) == Porta) = Jogador (x+1,y+1) dir caixa
                     | not caixa && dir == Oeste && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) && (getPeca m (x-1) (y+1) == Vazio || getPeca m (x-1) (y+1) == Porta) = Jogador (x-1,y+1) dir caixa
                     | caixa && dir == Este && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && (getPeca m (x+1) (y+1) == Vazio || getPeca m (x+1) (y+1) == Porta)
                                                                                                                  && (getPeca m (x+1) (y+2) == Vazio || getPeca m (x+1) (y+2) == Porta) = Jogador (x+1,y+1) dir caixa
                     | caixa && dir == Oeste && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) && (getPeca m (x-1) (y+1) == Vazio || getPeca m (x-1) (y+1) == Porta)
                                                                                                                   && (getPeca m (x-1) (y+2) == Vazio || getPeca m (x-1) (y+2) == Porta) = Jogador (x-1,y+1) dir caixa
                     | otherwise = j



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

interageCaixa :: Jogo -> Jogo  
interageCaixa jog@(Jogo m j@(Jogador (x,y) dir caixa)) | caixa && dir == Este && getPeca m (x+1) (y+1) == Vazio 
                                                                          && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) = Jogo (setPeca m Caixa (x+1) (y+1)) (Jogador (x,y) dir False)
                                                       | caixa && dir == Este && getPeca m (x+1) (y+1) == Vazio && getPeca m (x+1) y == Vazio = interageCaixa (Jogo m (Jogador (x,y-1) dir caixa))
                                                       | caixa && dir == Oeste && getPeca m (x-1) (y+1) == Vazio 
                                                                               && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) = Jogo (setPeca m Caixa (x-1) (y+1)) (Jogador (x,y) dir False)
                                                       | caixa && dir == Oeste && getPeca m (x-1) (y+1) == Vazio && getPeca m (x-1) y == Vazio = interageCaixa (Jogo m (Jogador (x,y-1) dir caixa))
                                                       | not caixa && dir == Este && getPeca m (x+1) y == Caixa && getPeca m x (y+1) == Vazio && getPeca m (x+1) (y+1) == Vazio = Jogo (setPeca m Vazio (x+1) y) (Jogador (x,y) dir True)
                                                       | not caixa && dir == Oeste && getPeca m (x-1) y == Caixa && getPeca m x (y+1) == Vazio && getPeca m (x-1) (y+1) == Vazio = Jogo (setPeca m Vazio (x-1) y) (Jogador (x,y) dir True)
                                                       | otherwise = jog
                                                   

-- Devolve o tipo de peca duma dada posicao no mapa
getPeca :: Mapa -> Int -> Int -> Peca
getPeca [] _ _ = Vazio
getPeca (h:t) x 0 = getLinha h x
getPeca (h:t) x y = getPeca t x (y-1)

-- Axuliar para a getPeca que devolve a peca de uma linha de acordo com uma coordenada x
getLinha :: [Peca] -> Int -> Peca
getLinha [] _ = Vazio
getLinha (h:t) 0 = h
getLinha (h:t) x = getLinha t (x-1)

-- Trocar uma peça numa posição do mapa para o tipo recebido no argumento
setPeca :: Mapa -> Peca -> Int -> Int -> Mapa 
setPeca [] _ _ _ = []
setPeca (h:t) p x 0 = setLinha h p x : t
setPeca (h:t) p x y = h : setPeca t p x (y-1)

setLinha :: [Peca] -> Peca -> Int -> [Peca]
setLinha [] _ _ = []
setLinha (h:t) p 0 = p:t
setLinha (h:t) p x = h : setLinha t p (x-1)