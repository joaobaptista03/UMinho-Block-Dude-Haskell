{- |
Module      : Tarefa6_2021li1g006
Description : Resolução de um puzzle
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}

module Tarefa6_2021li1g006 where

import Tarefa1_2021li1g006
import Tarefa2_2021li1g006
import Tarefa3_2021li1g006
import Tarefa4_2021li1g006
import Tarefa5_2021li1g006
import Data.Maybe

import LI12122
import qualified Data.Functor as Nothing


auxauxRS :: Jogo -> ([Maybe Movimento],Int)
auxauxRS (Jogo m (Jogador (x,y) Este c)) = (auxRS (Jogo m (Jogador (x,y) Este c)), length (auxRS (Jogo m (Jogador (x,y) Este c))))

auxRS :: Jogo -> [Maybe Movimento]
auxRS (Jogo m (Jogador (x,y) Este False)) | snd (saberPorta (desconstroiMapa m)) == (x,y) = []
auxRS (Jogo m (Jogador (x,y) Este False)) | getPeca m (x+1) y == Vazio && (getPeca m (x+1) (y+1) == Bloco || getPeca m (x+1) (y+1) == Caixa) = Just AndarDireita : auxRS (Jogo m (andaDirJogador (Jogador (x,y) Este False) m))
                                          | getPeca m (x+1) y == Vazio && getPeca m (x+1) (y+1) == Vazio && (getPeca m (x+1) (y+2) == Bloco || getPeca m (x+1) (y+2) == Caixa) = Just AndarDireita : auxRS (Jogo m (andaDirJogador (Jogador (x,y) Este False) m))
                                          | (getPeca m (x+1) y == Bloco || getPeca m (x+1) y == Caixa) && getPeca m (x+1) (y-1) == Vazio = Just Trepar : auxRS (Jogo m (trepa (Jogador (x,y) Este False) m))
                                          | getPeca m (x+1) y == Porta = Just AndarDireita : auxRS (Jogo m (andaDirJogador (Jogador (x,y) Este False) m))