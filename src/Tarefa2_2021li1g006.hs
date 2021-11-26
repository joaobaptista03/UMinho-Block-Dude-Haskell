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

-- |Dada uma lista, retorna uma lista com as peças sem as coordenadas.

-- |Por exemplo:

-- |pecas [(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,2)),(Bloco,(1,2)),(Caixa,(3,2))] = [Bloco,Bloco,Bloco,Bloco,Caixa]

-- |pecas [(Bloco,(4,1)),(Bloco,(2,1)),(Bloco,(5,1)),(Caixa,(1,1)),(Caixa,(3,1))] = [Bloco,Bloco,Bloco,Caixa,Caixa]
pecas :: [(Peca, Coordenadas)] -> [Peca]
pecas [] = []
pecas ((p, (x,y)) : t) = p : pecas t

-- |Retorna a lista esvaziada, dada pela função vaziomapa, ao contrário (por ordem crescente).

-- |Por exemplo:

-- |crescente [(Bloco,(0,2)),(Bloco,(1,3))] = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(0,3)),(Vazio,(1,3))]

-- |crescente [(Bloco,(0,2)),(Bloco,(1,3)),(Bloco,(1,1))] = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(0,3)),(Vazio,(1,3))]
crescente :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
crescente ((p,(x,y)):t) = reverse (vaziomapa ((p,(x,y)):t))

-- |Retorna a lista dada com Vazio em todas as coordenadas possíveis (por ordem decrescente).

-- |Por exemplo:

-- |vaziomapa [(Bloco,(0,1)),(Bloco,(1,1))] = [(Vazio,(1,1)),(Vazio,(0,1)),(Vazio,(1,0)),(Vazio,(0,0))]

-- |vaziomapa [(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,2))] = [(Vazio,(2,2)),(Vazio,(1,2)),(Vazio,(0,2)),(Vazio,(2,1)),(Vazio,(1,1)),(Vazio,(0,1)),(Vazio,(2,0)),(Vazio,(1,0)),(Vazio,(0,0))]
vaziomapa :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
vaziomapa [] = []
vaziomapa ((p,(x,y)):t) = let maxx = maximum (listax ((p,(x,y)):t))
                              maxy = maximum (listay ((p,(x,y)):t)) in
                                     esvaziarate (Vazio,(maxx,maxy))

-- |Retorna a lista dada esvaziada até a essa Peça, desde a linha 0 até essa, inclusivé (por ordem descrescente).

-- |Por exemplo:

-- |esvaziarate (Bloco,(1,1)) = [(Vazio,(1,1)),(Vazio,(0,1)),(Vazio,(1,0)),(Vazio,(0,0))]

-- |esvaziarate (Bloco,(3,2)) = [(Vazio,(3,2)),(Vazio,(2,2)),(Vazio,(1,2)),(Vazio,(0,2)),(Vazio,(3,1)),(Vazio,(2,1)),(Vazio,(1,1)),(Vazio,(0,1)),(Vazio,(3,0)),(Vazio,(2,0)),(Vazio,(1,0)),(Vazio,(0,0))]
esvaziarate :: (Peca, Coordenadas) -> [(Peca, Coordenadas)]
esvaziarate (_,(0,0))= [(Vazio,(0,0))]
esvaziarate (p,(x,y)) = if x > 0 && y >= 0 then esvaziarateaux (Vazio,(x,y)) ++ esvaziarate (Vazio,(x,y-1)) else if x < 0 && y < 0 then [] else []

-- |Retorna a lista dada esvaziada até a essa Peça, nessa linha apenas, inclusivé (por ordem descrescente).

-- |Por exemplo:

-- |esvaziarateaux (Bloco,(1,1)) = [(Vazio,(1,1)),(Vazio,(0,1))]

-- |esvaziarateaux (Bloco,(3,2)) = [(Vazio,(3,2)),(Vazio,(2,2)),(Vazio,(1,2)),(Vazio,(0,2))]
esvaziarateaux :: (Peca, Coordenadas) -> [(Peca, Coordenadas)]
esvaziarateaux (p,(0,y)) = [(Vazio,(0,y))]
esvaziarateaux (p,(x,y)) = if x >= 0 then (Vazio,(x,y)) : esvaziarateaux (Vazio,(x-1,y)) else []

-- |Une a lista original dada, com a lista de Vazios originada a partir dela.

-- |Por exemplo:

-- |juntar [(Bloco,(1,1))] (crescente [(Bloco,(1,1))]) = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(0,1)),(Bloco,(1,1))]

-- |juntar [(Bloco,(1,1)),(Bloco,(3,2))] (crescente [(Bloco,(1,1)),(Bloco,(3,2))]) = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(3,0)),(Vazio,(0,1)),(Bloco,(1,1)),(Vazio,(2,1)),(Vazio,(3,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(3,2))]
juntar :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
juntar [] l = l
juntar ((p, (x,y)) : t) ((p2, (x2,y2)) : t2) | (x,y) == (x2,y2) = (p, (x,y)) : juntar (ordPecasy (ordPecasx t)) t2
                                             | otherwise = (p2, (x2,y2)) : juntar (ordPecasy (ordPecasx ((p, (x,y)) : t))) t2

-- |Dada uma lista, ordena as peças por ordem crescente da coordenada x (Importada a função listax da Tarefa 1).

-- |Por exemplo:

-- |ordPecasx [(Bloco,(4,2)),(Bloco,(2,2)),(Bloco,(5,2)),(Bloco,(1,2)),(Caixa,(3,2))] = [(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(3,2)),(Bloco,(4,2)),(Bloco,(5,2))]

-- |ordPecasx [(Bloco,(4,1)),(Bloco,(2,1)),(Bloco,(5,1)),(Bloco,(1,1)),(Caixa,(3,1))] = [(Bloco,(1,1)),(Bloco,(2,1)),(Caixa,(3,1)),(Bloco,(4,1)),(Bloco,(5,1))]
ordPecasx :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordPecasx [] = []
ordPecasx ((p, (x,y)): t) = let minx = minimum (listax ((p, (x,y)) : t)) in
    if x == minx then (p, (x,y)) : ordPecasx t
    else ordPecasx (t ++ [(p, (x,y))])

-- |Dada uma lista, ordena as peças por ordem crescente da coordenada y (Importada a função listay da Tarefa 1).

-- |Por exemplo:

-- |ordPecasy [(Bloco,(4,5)),(Bloco,(2,2)),(Bloco,(5,3)),(Bloco,(1,2)),(Caixa,(3,2))] = [(Bloco,(2,2)),(Bloco,(1,2)),(Caixa,(3,2)),(Bloco,(5,3)),(Bloco,(4,5))]

-- |ordPecasy [(Bloco,(4,4)),(Bloco,(2,1)),(Bloco,(5,1)),(Bloco,(1,3)),(Caixa,(3,2))] = [(Bloco,(2,1)),(Bloco,(5,1)),(Caixa,(3,2)),(Bloco,(1,3)),(Bloco,(4,4))]
ordPecasy :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordPecasy [] = []
ordPecasy ((p, (x,y)): t) = let miny = minimum (listay ((p, (x,y)) : t)) in
    if y == miny then (p, (x,y)) : ordPecasy t
    else ordPecasy (t ++ [(p, (x,y))])

-- |Retorna a lista dada, com os Vazios adicionados.

-- |Por exemplo:

-- |listavazios [(Bloco,(1,1)),(Bloco,(3,2))] = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(3,0)),(Vazio,(0,1)),(Bloco,(1,1)),(Vazio,(2,1)),(Vazio,(3,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(3,2))]

-- |listavazios [(Bloco,(1,1)),(Bloco,(1,2))] = [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(0,1)),(Bloco,(1,1)),(Vazio,(0,2)),(Bloco,(1,2))]
listavazios :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listavazios ((p,(x,y)):t) = juntar ((p,(x,y)):t) (crescente ((p,(x,y)):t))

-- |Função que, perante um a = 0, dado na expressão constroiMapa, separa uma lista, já ordenada e com vazios, de (Peca, Coordenadas) por linhas (y = 0, y = 1, etc).

-- |Por exemplo:

-- |separaListas [(Bloco,(0,0)),(Bloco,(1,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Bloco,(0,2)),(Vazio,(1,2))] 0 = [[Bloco,Bloco],[Vazio,Vazio],[Bloco,Vazio]]

-- |separaListas [(Bloco,(0,0)),(Bloco,(1,0)),(Vazio,(0,1)),(Vazio,(1,1))] 0 = [[Bloco,Bloco],[Vazio,Vazio]]
separaListas :: [(Peca, Coordenadas)] -> Int -> [[Peca]]
separaListas ((p,(x,y)):t) a = if a < length ((p,(x,y)):t)
                              then take ((maximum (listax ((p,(x,y)):t))) + 1) (drop a (pecas ((p,(x,y)):t))) : separaListas ((p,(x,y)):t) (a + (maximum (listax ((p,(x,y)):t))) + 1)
                              else []

-- |Finalmente, a função final constroiMapa que "transforma" um conjunto de peças (não vazias e VÁLIDAS) em um mapa funcional com vazios, separado por linhas.

-- |Por exemplo:

-- |constroiMapa [(Bloco,(1,2)),(Bloco,(2,3)),(Bloco,(3,4))] = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio],[Vazio,Vazio,Vazio,Bloco]]

-- |constroiMapa [(Bloco,(1,2)),(Bloco,(2,3)),(Bloco,(3,3))] = [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Bloco,Bloco]]
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((p,(x,y)):t) = if validaPotencialMapa ((p,(x,y)):t) then separaListas (listavazios ((p,(x,y)):t)) 0 else []