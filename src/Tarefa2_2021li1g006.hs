{- |
Module      : Tarefa2_2021li1g006
Description : Construção/Desconstrução do mapa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g006 where

import LI12122
import Tarefa1_2021li1g006

-- |Cria uma lista da lista dada, sem a coluna c.

-- |Por exemplo:

-- |listaslinha 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))]

-- |listaslinha 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Caixa,(2,1))]
listaslinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listaslinha c [] = []
listaslinha c ((p, (x,y)) : t) | c == y = listaslinha c t
                               | otherwise = (p, (x,y)) : listaslinha c t

-- |Cria uma lista da lista (Mapa) dada, sem a coluna c.

-- |Por exemplo:

-- |constroiPecas 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Caixa,(2,1))]

-- |constroiPecas 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Bloco,(1,2)),(Bloco,(2,2))]
constroiPecas :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
constroiPecas y [] = []
constroiPecas y ((p,(x,y2)):t) = ordPecasx (if y == y2 then (p,(x,y2)) : constroiPecas y t else constroiPecas y t)

-- |Dada uma lista, ordena as peças por ordem crescente do argumento x (Importada a função listax da Tarefa 1).

-- |Por exemplo:

-- |ordPecasx [(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,2)),(Bloco,(1,2)),(Caixa,(3,2))] = [(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(3,2)),(Bloco,(4,2)),(Bloco,(5,2))]

-- |ordPecasx [(Bloco,(4,1)),(Bloco,(2,1)),(Bloco,(5,1)),(Bloco,(1,1)),(Caixa,(3,1))] = [(Bloco,(1,1)),(Bloco,(2,1)),(Caixa,(3,1)),(Bloco,(4,1)),(Bloco,(5,1))]
ordPecasx :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordPecasx [] = []
ordPecasx ((p, (x,y)): t) = let minx = minimum (listax ((p, (x,y)) : t)) in
    if x == minx then (p, (x,y)) : ordPecasx t
    else ordPecasx (t ++ [(p, (x,y))])

-- |Dada uma lista, retorna uma lista com as peças sem as coordenadas.

-- |Por exemplo:

-- |pecas [(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,2)),(Bloco,(1,2)),(Caixa,(3,2))] = [Bloco,Bloco,Bloco,Bloco,Bloco]

-- |pecas [(Bloco,(4,1)),(Bloco,(2,1)),(Bloco,(5,1)),(Bloco,(1,1)),(Caixa,(3,1))] = [Bloco,Bloco,Bloco,Bloco,Caixa]
pecas :: [(Peca, Coordenadas)] -> [Peca]
pecas [] = []
pecas ((p, (x,y)) : t) = p : pecas t

-- |Finalmente, a função que, juntando todas, constrói o mapa (Importada a função listay da Tarefa 1).

-- |Por exemplo:

-- |constroiMapa  [(Porta,(0,0)),(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(1,3)),(Bloco,(5,2)),(Bloco,(1,2)),(Bloco,(1,1)),(Caixa,(3,2))] = [[Porta],[Bloco],[Bloco,Bloco,Caixa,Bloco,Bloco],[Bloco]]
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((p, (x,y)) : t) = let miny = minimum (listay ((p, (x,y)) : t)) in
   pecas(constroiPecas miny ((p, (x,y)) : t)) : constroiMapa (listaslinha miny ((p, (x,y)) : t))

-- |Cria uma lista da coluna c, a partir da lista dada.

-- |Por exemplo:

-- |listaslinha 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))]

-- |listaslinha 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Caixa,(2,1))]
listalinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listalinha c [] = []
listalinha c ((p, (x,y)) : t) | c == y = (p, (x,y)) : listaslinha c t
                              | otherwise = listaslinha c t