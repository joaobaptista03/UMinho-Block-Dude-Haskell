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
  show = undefined


-- |Dada uma lista de Peças (de uma certa linha y = a), produz uma String com a sua representação no jogo.

-- |Por exemplo:

-- |pecapchar [Porta,Vazio,Vazio,Bloco] = "P  X"

-- |pecapchar [Bloco,Bloco,Bloco,Bloco] = "XXXX"
pecapchar :: [Peca] -> String
pecapchar [] = []
pecapchar (h:t) | h == Porta = "P" ++ pecapchar t
                | h == Bloco = "X" ++ pecapchar t
                | h == Caixa = "C" ++ pecapchar t
                | h == Vazio = " " ++ pecapchar t

-- |Dado um Mapa, produz a sua representação em jogo, sendo "\n" um parágrafo.

-- |Por exemplo:

-- |mapapchar [[Porta,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Caixa,Vazio,Vazio,Vazio],[Bloco,Caixa,Caixa,Caixa],[Bloco,Bloco,Bloco,Bloco]] =

-- |"P   \n    \n    \nC   \nXCCC\nXXXX\n"

-- |

-- |mapapchar [[Porta,Bloco,Bloco,Vazio],[Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Vazio,Vazio],[Caixa,Vazio,Vazio,Vazio],[Bloco,Caixa,Caixa,Caixa],[Bloco,Bloco,Bloco,Bloco]] =

-- |"PXX \n  X \n    \nC   \nXCCC\nXXXX\n"

mapapchar :: Mapa -> String
mapapchar [] = []
mapapchar (h:t) = pecapchar h ++ "\n" ++ mapapchar t


