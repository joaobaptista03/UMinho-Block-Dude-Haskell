{- |
Module      : Tarefa1_2021li1g006
Description : Validação de um potencial mapa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g006 where

import LI12122

--Tarefa 1 (2.1)

--Testa se as coordenadas "c" aparecem alguma vez no resto da lista (Mapa) dada. (f1 do Ponto 1)
elem' :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
elem' c [] = False
elem' c ((p,cs):t) | c == cs = True
                    | otherwise = elem' c t

--Testa, usando a função elem', se existe alguma coordenada repetida na lista (Mapa) dada. (f2 do Ponto 1)
repetido :: [(Peca, Coordenadas)] -> Bool
repetido [] = False
repetido [_] = False
repetido  ((p,cs):t) | elem' cs t = True
                      | otherwise = repetido t

--Conta quantas portas existem na lista (Mapa) dada. (f1 do Ponto 2)
quantasPortas :: [(Peca, Coordenadas)] -> Int
quantasPortas [] = 0
quantasPortas ((p,cs):t) | p == Porta = 1 + quantasPortas t
                        | otherwise = quantasPortas t

--Testa, usando a função quantasPortas, se apenas existe 1 porta na lista (Mapa) dada. (f2 do Ponto 2)
so1Porta :: [(Peca, Coordenadas)] -> Bool
so1Porta ((p,cs):t) = quantasPortas ((p,cs):t) == 1

--Dada uma lista (Mapa), produz uma lista com todas as caixas da mesma. (f1 do Ponto 3)
listaCaixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listaCaixas [] = []
listaCaixas ((p,cs):t) | p == Caixa = (p,cs) : listaCaixas t
                       | otherwise = listaCaixas t

--Dada uma caixa, verifica se essa caixa está em cima de outra caixa ou de um bloco, e se não se encontra a flutuar. (f2 do Ponto 3)
testaCaixa :: [(Peca, Coordenadas)] -> (Peca, Coordenadas) -> Bool
testaCaixa [] (Caixa,(x,y)) = False
testaCaixa ((p,(x2,y2)):t) (Caixa,(x,y)) | (p == Caixa || p == Bloco) && x == x2 && y+1 == y2 = True
                                         | otherwise = testaCaixa t (Caixa,(x,y))

--Dada uma lista (Mapa), testa todas as caixas usando as funções testaCaixa e listaCaixas, usando "all" para verificar se todas as caixas são válidas. (f3 do Ponto 3)
verificaCaixas :: [(Peca, Coordenadas)] -> Bool
verificaCaixas [] = True
verificaCaixas ((p,cs):t) = let caixas = listaCaixas ((p,cs):t) in
    all (testaCaixa ((p,cs):t)) (listaCaixas ((p,cs):t))

--Cria uma lista das coordenadas y da lista (Mapa) dada. (f1 do Ponto 4)
listay :: [(Peca, Coordenadas)] -> [Int]
listay [] = []
listay ((p, (x,y)) : t) = y : listay t

--Cria uma lista das coordenadas x da lista (Mapa) dada. (f2 do Ponto 4)
listax :: [(Peca, Coordenadas)] -> [Int]
listax [] = []
listax ((p, (x,y)) : t) = x : listax t

haespaco :: [(Peca, Coordenadas)] -> Bool
haespaco [] = False
haespaco ((p,cs):t) = let area = (maximum (listay ((p,cs):t)) + 1) * (maximum (listax ((p,cs):t)) + 1) in
    length ((p,cs):t) < area

--Testa a validade do mapa pelos pontos 1 a 5 da Tarefa 1.
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa [(p,cs)] = True
validaPotencialMapa l = not (repetido l) && so1Porta l && verificaCaixas l && haespaco l