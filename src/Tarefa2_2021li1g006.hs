{- |
Module      : Tarefa2_2021li1g006
Description : Construção/Desconstrução do mapa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g006 where

import LI12122

-- |Cria uma lista das coordenadas y da lista (Mapa) dada.

-- |Por exemplo:

-- |listay [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [0,2,2,1]

-- |listay [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,0))] = [0,2,2,0]
listay :: [(Peca, Coordenadas)] -> [Int]
listay [] = []
listay ((p, (x,y)) : t) = y : listay t

-- |Cria uma lista da lista (Mapa) dada, sem a coluna c.

-- |Por exemplo:

-- |listaslinha 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))]

-- |listaslinha 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Caixa,(2,1))]
listaslinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listaslinha c [] = []
listaslinha c ((p, (x,y)) : t) | c == y = listaslinha c t
                                | otherwise = (p, (x,y)) : (listaslinha c t)

-- |Cria uma lista da lista (Mapa) dada, sem a coluna c.

-- |Por exemplo:

-- |constroiPecas 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [Caixa]

-- |constroiPecas 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [Bloco,Bloco]
constroiPecas :: Int -> [(Peca, Coordenadas)] -> [Peca]
constroiPecas y [] = []
constroiPecas y ((p,(x,y2)):t) = if y == y2 then p : constroiPecas y t else constroiPecas y t

-- Finalmente, a função que, juntando todas, constrói o mapa.
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((p, (x,y)) : t) = let miny = minimum (listay ((p, (x,y)) : t)) in
    constroiPecas miny ((p, (x,y)) : t) : constroiMapa (listaslinha miny ((p, (x,y)) : t) )