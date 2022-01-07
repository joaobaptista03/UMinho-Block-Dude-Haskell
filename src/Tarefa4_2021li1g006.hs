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
import Tarefa1_2021li1g006
import Tarefa2_2021li1g006

-- |Aplica o efeito de um comando sobre o jogador.

-- |Por exemplo:

-- |moveJogador m1e1 AndarEsquerda = Jogo m1r (Jogador (5, 3) Oeste False)
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m j) AndarDireita = Jogo m (andaDirJogador j m)
moveJogador (Jogo m j) AndarEsquerda = Jogo m (andaEsqJogador j m)
moveJogador (Jogo m j) Trepar = Jogo m (trepa j m)
moveJogador j InterageCaixa = interageCaixa j

-- |Aplica consecutivamente os comandos dados pela lista.

-- |Por exemplo:

-- |correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda] = m1e2
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) = correrMovimentos (moveJogador jogo h) t


-- |A função permite ao jogador trepar, se possível, o obstáculo à sua frente.

-- |Por exemplo:

-- |trepa (Jogador (3, 3) Este False) m1r = (Jogador (4, 2) Este False) 

-- |trepa (Jogador (2, 3) Este False) m1r = (Jogador (2, 3) Este False)
trepa :: Jogador -> Mapa -> Jogador
trepa j@(Jogador (x,y) dir caixa) m
                     | not caixa && dir == Este && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && (getPeca m (x+1) (y-1) == Vazio || getPeca m (x+1) (y-1) == Porta) = Jogador (x+1,y-1) dir caixa
                     | not caixa && dir == Oeste && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) && (getPeca m (x-1) (y-1) == Vazio || getPeca m (x-1) (y-1) == Porta) = Jogador (x-1,y-1) dir caixa
                     | caixa && dir == Este && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && (getPeca m (x+1) (y-1) == Vazio || getPeca m (x+1) (y-1) == Porta)
                                                                                                                  && getPeca m (x+1) (y-2) == Vazio = Jogador (x+1,y-1) dir caixa
                     | caixa && dir == Oeste && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) && (getPeca m (x-1) (y-1) == Vazio || getPeca m (x-1) (y-1) == Porta)
                                                                                                                   && getPeca m (x-1) (y-2) == Vazio = Jogador (x-1,y-1) dir caixa
                     | otherwise = j

-- |Faz com que o jogador se volte para Este e avance, se for possível, uma unidade.

-- |Por exemplo:

-- |andaDirJogador (Jogador (2, 3) Oeste False) m1r = (Jogador (3, 3) Oeste False)

-- |andaDirJogador (Jogador (6, 0) Oeste False) m1r = (Jogador (6, 0) Este False) (pois está no limite do mapa)
andaDirJogador :: Jogador -> Mapa -> Jogador
andaDirJogador (Jogador (x,y) dir caixa) m
             | x == length (head m) = Jogador (x,y) dir caixa
             | getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa = Jogador (x,y) Este caixa
             | getPeca m (x+1) y == Porta = if caixa then if getPeca m (x+1) (y-1) /= Vazio then Jogador (x,y) Este caixa else Jogador (x+1,y) Este caixa else Jogador (x+1,y) Este caixa
             | otherwise = if getPeca m (x+1) (y+1) == Bloco || getPeca m (x+1) (y+1) == Caixa then if caixa then if getPeca m (x+1) (y-1) /= Vazio then Jogador (x,y) Este caixa else Jogador (x+1,y) Este caixa else Jogador (x+1,y) Este caixa
             else if getPeca m (x+1) (y+1) == Porta then Jogador (x+1,y-1) Este caixa
             else andaDirJogador (Jogador (x,y+1) Este caixa) m

-- |Faz com que o jogador se volte para Oeste e avance, se for possível, uma unidade.

-- |Por exemplo:

-- |andaEsqJogador (Jogador (2, 3) Oeste False) m1r = (Jogador (1, 3) Oeste False)

-- |andaEsqJogador (Jogador (5, 3) Este False) m1r = (Jogador (5, 3) Oeste False)  (pois existe um obstáculo)
andaEsqJogador :: Jogador -> Mapa -> Jogador
andaEsqJogador (Jogador (x,y) dir caixa) m
             | x == 0 = Jogador (x,y) dir caixa
             | getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa = Jogador (x,y) Oeste caixa
             | getPeca m (x-1) y == Porta = if caixa then if getPeca m (x-1) (y-1) /= Vazio then Jogador (x,y) Oeste caixa else Jogador (x-1,y) Oeste caixa else Jogador (x-1,y) Oeste caixa
             | otherwise = if getPeca m (x-1) (y+1) == Bloco || getPeca m (x-1) (y+1) == Caixa then if caixa then if getPeca m (x-1) (y-1) /= Vazio then Jogador (x,y) Oeste caixa else Jogador (x-1,y) Oeste caixa else Jogador (x-1,y) Oeste caixa
             else if getPeca m (x-1) (y+1) == Porta then Jogador (x-1,y-1) Oeste caixa
             else andaEsqJogador (Jogador (x,y+1) Oeste caixa) m


-- |A função permite ao jogador carregar/largar uma caixa.

-- |Por exemplo:

-- |interageCaixa  (Jogador (3, 3) Este False) m1r =  (Jogador (3, 3) Este True) m1r

-- |interageCaixa  (Jogador (2, 3) Este False) m1r =  (Jogador (2, 3) Este False) m1r (pois não existe caixa para apanhar ou largar)

-- |interageCaixa  (Jogador (3, 3) Este True) m1r = (Jogador (3, 3) Este False) m1r (pousou uma caixa)
interageCaixa :: Jogo -> Jogo
interageCaixa jog@(Jogo m j@(Jogador (x,y) dir caixa)) 
                                                       | caixa && dir == Este && getPeca m (x+1) (y-1) == Vazio && (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && getPeca m x (y-1) == Vazio
                                                        = Jogo (setPeca m Caixa (x+1) (y-1)) (Jogador (x,y) dir False)
                                                       | caixa && dir == Oeste && getPeca m (x-1) (y-1) == Vazio && (getPeca m (x-1) y == Bloco || getPeca m (x-1) y == Caixa) && getPeca m x (y-1) == Vazio
                                                        = Jogo (setPeca m Caixa (x-1) (y-1)) (Jogador (x,y) dir False)
                                                       | caixa && dir == Este && getPeca m (x+1) (y-1) == Vazio && getPeca m x (y-1) == Vazio && getPeca m (x+1) y == Vazio && (getPeca m (x+1) (y+1) == Bloco || getPeca m (x+1) (y+1) == Caixa)
                                                        = Jogo (setPeca m Caixa (x+1) y) (Jogador (x,y) dir False)
                                                       | caixa && dir == Oeste && getPeca m (x-1) (y-1) == Vazio && getPeca m x (y-1) == Vazio && getPeca m (x-1) y == Vazio && (getPeca m (x-1) (y+1) == Bloco || getPeca m (x-1) (y+1) == Caixa)
                                                        = Jogo (setPeca m Caixa (x-1) y) (Jogador (x,y) dir False)
                                                       | caixa && dir == Oeste && getPeca m (x-1) (y-1) == Vazio && getPeca m x (y-1) == Vazio && getPeca m (x-1) y == Vazio = Jogo (setPeca m Caixa (x-1) ((findChao (x-1) y m) - 1)) (Jogador (x,y) dir False)
                                                       | caixa && dir == Este && getPeca m (x+1) (y-1) == Vazio && getPeca m x (y-1) == Vazio && getPeca m (x+1) y == Vazio = Jogo (setPeca m Caixa (x+1) ((findChao (x+1) y m) - 1)) (Jogador (x,y) dir False) 
                                                       | not caixa && dir == Este && getPeca m (x+1) y == Caixa && getPeca m x (y-1) == Vazio && getPeca m (x+1) (y-1) == Vazio = Jogo (setPeca m Vazio (x+1) y) (Jogador (x,y) dir True)
                                                       | not caixa && dir == Oeste && getPeca m (x-1) y == Caixa && getPeca m x (y-1) == Vazio && getPeca m (x-1) (y-1) == Vazio = Jogo (setPeca m Vazio (x-1) y) (Jogador (x,y) dir True)
                                                       | otherwise = jog

-- | Dado um mapa e um inteiro (coordenada x), calcula a lista das coordenadas y's correspondentes a esse x.
listaColuna :: [(Peca,Coordenadas)] -> Int -> [Int]
listaColuna [] _ = []
listaColuna ((p,(x,y)):t) a = if a == x then y : listaColuna t a else listaColuna t a

-- | Juntamente com a função listaColuna, calcula o y mais acima, tendo também de ser abaixo do y dado.
findChao :: Int -> Int -> Mapa -> Int
findChao x y m = minimum $ filter (> y) (listaColuna (desconstroiMapa m) x)


-- |A função getPeca devolve o tipo de peca duma dada posicao no mapa.

-- |Por exemplo:

-- |getPeca m1r 0 3 = Porta
getPeca :: Mapa -> Int -> Int -> Peca
getPeca [] _ _ = Vazio
getPeca (h:t) x 0 = getLinha h x
getPeca (h:t) x y = getPeca t x (y-1)

-- Axuliar para a getPeca que devolve a peca de uma linha de acordo com uma coordenada x.

-- |Por exemplo:

-- |getLinha [Bloco,Bloco,Porta] 3 = Porta   
getLinha :: [Peca] -> Int -> Peca
getLinha [] _ = Vazio
getLinha (h:t) 0 = h
getLinha (h:t) x = getLinha t (x-1)

-- |Trocar uma peça numa posição do mapa para o tipo recebido no argumento.

-- |Por exemplo: 

-- |setPeca [[Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]] Vazio 2 1 = [[Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]]   

-- |setPeca [[Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]] Vazio 0 2 = [[Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Bloco,Bloco]]   
setPeca :: Mapa -> Peca -> Int -> Int -> Mapa
setPeca [] _ _ _ = []
setPeca (h:t) p x 0 = setLinha h p x : t
setPeca (h:t) p x y = h : setPeca t p x (y-1)

-- |Auxiliar da SetPeca que altera a peça de uma certa linha para o tipo de peça recebido como argumento, mantendo igual o resto da linha.

-- |Por exemplo:

-- |setLinha [Vazio,Vazio,Caixa,Bloco] Vazio 2 = [Vazio,Vazio,Vazio,Bloco]    
setLinha :: [Peca] -> Peca -> Int -> [Peca]
setLinha [] _ _ = []
setLinha (h:t) p 0 = p:t
setLinha (h:t) p x = h : setLinha t p (x-1)