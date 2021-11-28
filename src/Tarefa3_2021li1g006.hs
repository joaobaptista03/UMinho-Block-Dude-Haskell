{- |
Module      : Tarefa3_2021li1g006
Description : Representação textual do jogo
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g006 where

import LI12122

instance Show Jogo where
  show = mostrar


-- |Esta função retorna a string "X" se a Peça for um Bloco, "C" se for Caixa, "P" se for uma porta, e " " se for Vazio.
peca1 :: Peca -> String
peca1 p | p == Bloco = "X"
        | p == Caixa = "C"
        | p == Porta = "P"
        | p == Vazio = " "


auxiliar :: String -> Coordenadas -> String
auxiliar (h:t) (x,y) | x == 0 && y == 0 = 'C' : t
                        | x > 1 && h == '/' = h : auxiliar t (x-1,y)
                        | x > 1 && h == 'n' = h : auxiliar t (x,y)
                        | x > 1 = h : auxiliar t (x,y)

caixa :: Coordenadas -> Coordenadas 
caixa (x,y) = (x,y-1)

