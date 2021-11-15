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

--Testa se as coordenadas "c" aparecem alguma vez no resto da lista (Mapa) dada.
elem' :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
elem' c [] = False
elem' c ((a1,b1):t) | c == b1 = True
                    | otherwise = elem' c t

--Testa, usando a função elem', se existe alguma coordenada repetida na lista (Mapa) dada.
repetido :: [(Peca, Coordenadas)] -> Bool
repetido [] = False
repetido [_] = False
repetido  ((a1,b1):t) | elem' b1 t = True
                      | otherwise = repetido t

--Testa a validade do mapa pelos pontos 1 a 5 da Tarefa 1.
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = True
validaPotencialMapa [(a1,b1)] = True
validaPotencialMapa ((a1,b1):t) | repetido ((a1,b1):t) = False 
                                | otherwise = True