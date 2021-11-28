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

-- |Esta função retorna a string "<" se o jogador estiver virado para a esquerda (Oeste), e ">" em caso contrário.
direcao1 :: Direcao -> String
direcao1 d | d == Oeste = "<"
           | d == Este = ">"

mostrar1 :: Jogo -> String
mostrar1 (Jogo [] (Jogador (x,y) _ _)) = ""
mostrar1 (Jogo ([]:t) (Jogador (x,y) d c)) = "\n" ++ mostrar1 (Jogo t (Jogador (x,y-1) d c))
mostrar1 (Jogo ((p:ps):t) (Jogador (x,y) d c)) | x == 0 && y == 0 = direcao1 d ++ mostrar1 (Jogo (ps:t) (Jogador (-1,-1) d c))
                                                              | y == 0 = peca1 p ++ mostrar1 (Jogo (ps:t) (Jogador (x-1,y) d c))
                                                              | otherwise = peca1 p ++ mostrar1 (Jogo (ps:t) (Jogador (x,y) d c))

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

mostrar :: Jogo -> String 
mostrar j@(Jogo _ (Jogador (x,y) _ c)) | c = auxiliar (mostrar1 j) (caixa (x,y))
                                       | otherwise = mostrar1 j