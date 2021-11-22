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

-- |Testa se as coordenadas "c" aparecem alguma vez no resto da lista dada.

-- |Por exemplo:

-- |elem' (1,2) [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))] = True

-- |elem' (1,3) [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))] = False
elem' :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
elem' c [] = False
elem' c ((p,cs):t) | c == cs = True
                   | otherwise = elem' c t

-- |Testa, usando a função elem', se existe alguma coordenada repetida na lista dada.

-- |Por exemplo:

-- |repetido [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(1,2))] = True

-- |repetido [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))] = False
repetido :: [(Peca, Coordenadas)] -> Bool
repetido [] = False
repetido [_] = False
repetido  ((p,cs):t) | elem' cs t = True
                     | otherwise = repetido t

-- |Conta quantas portas existem na lista dada.

-- |Por exemplo:

-- |quantasPortas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))] = 1

-- |quantasPortas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Porta,(0,1))] = 2
quantasPortas :: [(Peca, Coordenadas)] -> Int
quantasPortas [] = 0
quantasPortas ((p,cs):t) | p == Porta = 1 + quantasPortas t
                         | otherwise = quantasPortas t

-- |Dada uma lista, retorna o primeiro conjunto (Porta, Coordenadas).

-- |Por exemplo:

-- |saberPorta [(Porta,(1,1)),(Bloco,(1,2)),(Bloco,(2,2))] = (Porta,(1,1))

-- |saberPorta [(Porta,(4,1)),(Bloco,(1,2)),(Bloco,(2,2))] = (Porta,(4,1))
saberPorta :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
saberPorta ((p,(x,y)):t) | p == Porta = (p,(x,y))
                         | otherwise = saberPorta t

-- |Verifica, usando a função quantasPortas, se apenas existe 1 porta na lista dada.

-- |Por exemplo:

-- |so1Porta [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2))] = True

-- |so1Porta [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Porta,(0,1))] = False
so1Porta :: [(Peca, Coordenadas)] -> Bool
so1Porta ((p,cs):t) = quantasPortas ((p,cs):t) == 1

-- |Dada uma lista, produz uma outra lista com todas as caixas da mesma.

-- |Por exemplo:

-- |listaCaixas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Caixa,(2,1))]]

-- |listaCaixas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1)),(Caixa,(1,1))] = [(Caixa,(2,1)),(Caixa,(1,1))]
listaCaixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listaCaixas [] = []
listaCaixas ((p,cs):t) | p == Caixa = (p,cs) : listaCaixas t
                       | otherwise = listaCaixas t

-- |Dada uma caixa, verifica se essa caixa está em cima de outra caixa ou de um bloco, e se não se encontra a flutuar.

-- |Por exemplo:

-- |testaCaixa [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] (Caixa,(2,1)) = True

-- |testaCaixa [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,0))] (Caixa,(2,0)) = False
testaCaixa :: [(Peca, Coordenadas)] -> (Peca, Coordenadas) -> Bool
testaCaixa [] (Caixa,(x,y)) = False
testaCaixa ((p,(x2,y2)):t) (Caixa,(x,y)) | (p == Caixa || p == Bloco) && x == x2 && y+1 == y2 = True
                                         | otherwise = testaCaixa t (Caixa,(x,y))

-- |Dada uma lista, testa todas as caixas usando as funções testaCaixa e listaCaixas, usando "all" para verificar se todas as caixas são válidas.

-- |Por exemplo:

-- |verificaCaixas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = True

-- |verificaCaixas [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,0))] = False
verificaCaixas :: [(Peca, Coordenadas)] -> Bool
verificaCaixas [] = True
verificaCaixas ((p,cs):t) = all (testaCaixa ((p,cs):t)) (listaCaixas ((p,cs):t))
    
-- |Cria uma lista das coordenadas y da lista dada.

-- |Por exemplo:

-- |listay [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [0,2,2,1]

-- |listay [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,0))] = [0,2,2,0]
listay :: [(Peca, Coordenadas)] -> [Int]
listay [] = []
listay ((p, (x,y)) : t) = y : listay t

-- |Cria uma lista das coordenadas x da lista dada.

-- |Por exemplo:

-- |listax [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [0,1,2,2]

-- |listax [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(3,0))] = [0,1,2,3]
listax :: [(Peca, Coordenadas)] -> [Int]
listax [] = []
listax ((p, (x,y)) : t) = x : listax t

-- |Cria uma lista da lista dada, sem a coluna c.

-- |Por exemplo:

-- |listascoluna 2 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(1,2))]

-- |listascoluna 1 [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(2,2)),(Caixa,(2,1))]
listascoluna :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listascoluna c [] = []
listascoluna c ((p, (x,y)) : t) | c == x = listascoluna c t
                                | otherwise = (p, (x,y)) : (listascoluna c t)

-- |Cria uma lista da lista dada, sem a coluna c.

-- |Por exemplo:

-- |listaBlocos [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Bloco,(1,2)),(Bloco,(2,2))]
listaBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
listaBlocos [] = []
listaBlocos ((p, (x,y)) : t) | p == Bloco = ((p, (x,y)) : listaBlocos t)
                             | otherwise = listaBlocos t

-- |Determina se existe pelo menos um espaço vazio no mapa.

-- |Por exemplo:

-- |haespaco [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = True

-- |haespaco [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Caixa,(1,0))] = False
haespaco :: [(Peca, Coordenadas)] -> Bool
haespaco [] = False
haespaco ((p,cs):t) = let area = (maximum (listay ((p,cs):t)) + 1) * (maximum (listax ((p,cs):t)) + 1) in
    length ((p,cs):t) < area

-- |Dada uma lista vertical, devolve o conjunto (Peca, Coordenadas) que estiver mais abaixo (>y).

-- |Por exemplo:

-- |maxfunc [(Porta,(0,0)),(Bloco,(1,1)),(Bloco,(2,2)),(Caixa,(2,1))] = (Bloco,(2,2))

-- |maxfunc [(Porta,(0,0)),(Bloco,(1,1)),(Bloco,(2,2)),(Caixa,(2,3))] = (Caixa,(2,3))
maxfunc :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
maxfunc [(p,(x,y))] = (p,(x,y))
maxfunc ((p,(x,y)):t) = let maxy = maximum (listay ((p,(x,y)):t)) in
    if y == maxy && p == Bloco then (p,(x,y))
    else maxfunc t

-- |Dada uma lista, produz outra lista com os elementos da mesma coluna do primeiro elemento seguidos na lista.

-- |Por exemplo:

-- |func' [(Bloco,(0,2)), (Bloco,(1,2)), (Bloco,(2,2)), (Bloco,(3,2)), (Bloco,(1,1)),(Bloco,(3,1)),(Bloco,(0,0))] = [(Bloco,(0,2))]

-- |func' [(Bloco,(0,2)), (Bloco,(0,0)), (Bloco,(1,2)), (Bloco,(2,2)), (Bloco,(3,2)), (Bloco,(1,1)),(Bloco,(3,1))] = [(Bloco,(0,2)),(Bloco,(0,0))]
func' :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
func' [] = []
func' [(p,(x,y))] = [(p,(x,y))]
func' ((p,(x,y)):(p2,(x2,y2)):t) | x == x2 = (p,(x,y)) : func' ((p2,(x2,y2)):t)
                                 | otherwise = [(p,(x,y))]

-- |Dada uma lista, produz outra lista de blocos com a coluna mais à direita do mapa, usando a func'.

-- |Por exemplo:

-- |func [(Bloco,(0,2)), (Bloco,(1,2)), (Bloco,(2,2)), (Bloco,(3,2)), (Bloco,(1,1)),(Bloco,(3,1)),(Bloco,(0,0))] = [(Bloco,(3,2)),(Bloco,(3,1))]

-- |func [(Bloco,(0,2)), (Bloco,(3,0)), (Bloco,(1,2)), (Bloco,(2,2)), (Bloco,(3,2)), (Bloco,(1,1)),(Bloco,(3,1)),(Bloco,(0,0))] = [(Bloco,(3,0)),(Bloco,(3,2)),(Bloco,(3,1))]
func :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
func [] = []
func ((p,(x,y)):t) = func'( let maxx = maximum (listax ((p,(x,y)):t)) in
    if x == maxx && p == Bloco then (p,(x,y)) : func t
    else func t)

-- |Dada uma peça, remove (caso haja) a primeira vez que ela aparece na lista dada.

-- |Por exemplo:

-- |deletePeca (Porta,(0,0)) [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))]

-- |deletePeca (Bloco,(2,2)) [(Porta,(0,0)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))] = [(Porta,(0,0)),(Bloco,(1,2)),(Caixa,(2,1))]
deletePeca :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
deletePeca _ [] = []
deletePeca p (h:t) | elem p (h:t) = if p == h then t else h : deletePeca p t
                   | otherwise = h:t

-- |Finalmente, valida o chão. (são necessárias as condições das ll. 218/219 pois a Porta fazia conflito com a função!)

-- |Por exemplo:

-- |validoChao [(Porta, (0,0)), (Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,5)),(Caixa,(0,4)),(Caixa,(1,4)),(Caixa,(2,4)),(Caixa,(3,4))] = True
validoChao :: [(Peca, Coordenadas)] -> Bool
validoChao [(Bloco,(0,y))] = True
validoChao l = let (Bloco,(x,y)) = maxfunc(func l) in
    if filter (/=0) (listax l) == [] then True
    else if elem (Bloco,(x-1,y)) (listaBlocos l) || elem (Bloco,(x-1,y+1)) (listaBlocos l) || elem (Bloco,(x-1,y-1)) (listaBlocos l) then validoChao (listascoluna x (listaBlocos l))
    else False


-- |Testa a validade do mapa pelos pontos 1 a 5 da Tarefa 1 (com a adição da função evalidaPorta).
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa [(p,cs)] = True
validaPotencialMapa l = not (repetido l) && so1Porta l && verificaCaixas l && haespaco l && validoChao l