{- |
Module      : Tarefa5_2021li1g006
Description : Aplicação gráfica completa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where

import Tarefa1_2021li1g006
import Tarefa2_2021li1g006
import Tarefa3_2021li1g006
import Tarefa4_2021li1g006
import LI12122
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

type Estado = (MenuJogo, EstadoJogo)

data MenuJogo = Menu | Jogar
inicio :: MenuJogo
inicio = Menu

type EstadoJogo = (Int,Int)

jogoInicial :: Estado
jogoInicial = (Jogar,(0,0))

window :: Display
window = InWindow "Block Dude - Joao Baptista e Mariana Pinto" (400, 400) (760,340)

playlogo :: Picture 
playlogo = pictures
    [ translate 0 50 $ color black $ rectangleSolid 102 27
    , translate 0 50 $ color white $ rectangleSolid 100 25
    , translate (-14) 43 $ scale 0.13 0.13 $ text "Play"]

reageEvento :: Event -> Estado -> Estado
reageEvento e (Menu,(a,b)) = reageEventoMenu e (Menu,(a,b))
reageEvento e (Jogar,(a,b)) = reageEventoJogo e (Jogar,(a,b))

reageEventoJogo :: Event -> Estado -> Estado
reageEventoJogo e (Jogar,(x,y)) = (Jogar,(x,y))

reageEventoMenu :: Event -> Estado -> Estado 
reageEventoMenu e@(EventKey (MouseButton LeftButton) Down _ (x,y)) (Menu,(a,b)) = if x >= (-51) && x <= 51 && y >= (-13.5) && y <= 13.5 then reageEventoJogo e jogoInicial else (Menu,(a,b))

main:: IO ()
main = do
    play window
      white
      50
      mainInicial
      reageEvento