{- |
Module      : Tarefa5_2021li1g006
Description : Aplicação gráfica completa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Exit
import Tarefa1_2021li1g006
import Tarefa2_2021li1g006
import Tarefa3_2021li1g006
import Tarefa4_2021li1g006
import LI12122

data Jogoo = Jogoo (Int, Int) [(Int, Int)] Direcao 

data Opcoes = Jogar
            | Sair

data Menu = Controlador Opcoes
          | ModoJogo Jogoo
          | VenceuJogo

type World = (Menu, Jogoo)

window :: Display
window = InWindow "Jumper Dude - by Joao Baptista e Mariana Pinto" (552, 400) (670,320)

fr :: Int
fr = 50

playlogo :: String -> Picture 
playlogo texto = pictures
    [ translate 0 50 $ color black $ rectangleSolid 204 54
    , translate 0 50 $ color (greyN 0.8) $ rectangleSolid 200 50
    , translate (-29) 38 $ scale 0.26 0.26 $ text texto]



draw :: Picture -> Picture -> Picture -> Picture -> World -> Picture
draw logo caixa playerL playerR (VenceuJogo, jogo) = Pictures [Translate (-280) (-40) $ Color (dark green) $ translate 60 10 $ scale 0.7 0.7 $ Text "It's a Win!", translate (-230) (150) $ scale 0.3 0.3 logo]
draw logo caixa playerL playerR (Controlador Jogar, jogo) = Pictures [Color (bright magenta) $ Translate (-110) (-100) $ playlogo "Play", Translate 110 (-100) $ playlogo "Exit", translate 0 100 $ scale 0.5 0.5 logo]
draw logo caixa playerL playerR (Controlador Sair, jogo) = Pictures [Translate (-110) (-100) $ playlogo "Play", Color (makeColor 0 212 255 1) $ Translate 110 (-100) $ playlogo "Exit", translate 0 100 $ scale 0.5 0.5 logo]
draw logo caixa playerL playerR (ModoJogo (Jogoo (x, y) l d), jogo) = Pictures $ map (caixas caixa) l ++ (if d == Oeste then [Translate i j $ scale 0.1 0.1 playerL] else [Translate i j $ scale 0.1 0.1 playerR]) ++ [translate (-212) (154) $ scale 0.3 0.3 logo]
  where
    i = fromIntegral x
    j = fromIntegral y

caixas :: Picture -> (Int, Int) -> Picture
caixas pic (x, y) = Translate i j $ scale 0.3955 0.3955 pic 
    where
      i = fromIntegral x
      j = fromIntegral y


drawOption :: String -> Picture
drawOption option = Translate (-60) 20 $ Scale 0.5 0.5 $ Text option

event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo) = (ModoJogo jogo, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo) = undefined
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo) = (Controlador Jogar, jogo)
event _ (ModoJogo (Jogoo (x, y) [] d), jogo) = (VenceuJogo, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (Jogoo (x, y) l d), jogo) | (x,y + 50) `elem` l = (ModoJogo (Jogoo (x, y) l d), jogo)
                                                                                 | otherwise = if d == Oeste then 
                                                                                                              if (x - 50, y) `elem` l && (x - 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x - 50, y + 50) l Oeste), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l d), jogo)
                                                                                                else if (x + 50, y) `elem` l && (x + 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x + 50, y + 50) l Este), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l d), jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo (Jogoo (x, y) l d), jogo) = if d == Oeste then 
                                                                                                      if (x - 50, y - 100) `elem` l && (x - 50, y) `notElem` l && (x - 50, y - 50) `notElem` l
                                                                                                        then (ModoJogo (Jogoo (x - 50, y - 50) l d), jogo)
                                                                                                          else (ModoJogo (Jogoo (x, y) l d), jogo)
                                                                                                      else if (x + 50, y - 100) `elem` l && (x + 50, y) `notElem` l && (x + 50, y - 50) `notElem` l
                                                                                                            then (ModoJogo (Jogoo (x + 50, y - 50) l d), jogo)
                                                                                                          else (ModoJogo (Jogoo (x, y) l d), jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (Jogoo (x, y) l d), jogo) | (x - 50,y) `elem` l = if (x,y + 50) `elem` l then (ModoJogo (Jogoo (x, y) l Oeste), jogo)
                                                                                                           else if d == Oeste then 
                                                                                                              if (x - 50, y) `elem` l && (x - 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x - 50, y + 50) l Oeste), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l Oeste), jogo)
                                                                                                              else if (x + 50, y) `elem` l && (x + 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x + 50, y + 50) l Oeste), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l Oeste), jogo)
                                                                                   | otherwise = if (x - 50,y - 50) `notElem` l && (x - 50,y - 100) `elem` l then (ModoJogo (Jogoo (x - 50, y - 50) l Oeste), jogo)
                                                                                                  else (ModoJogo (Jogoo (x-50, y) l Oeste), jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (Jogoo (x, y) l d), jogo) | (x + 50,y) `elem` l = if (x,y + 50) `elem` l then (ModoJogo (Jogoo (x, y) l Este), jogo)
                                                                                                           else if d == Este then 
                                                                                                              if (x + 50, y) `elem` l && (x + 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x + 50, y + 50) l Este), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l Este), jogo)
                                                                                                              else if (x - 50, y) `elem` l && (x - 50, y + 50) `notElem` l
                                                                                                                then (ModoJogo (Jogoo (x - 50, y + 50) l Este), jogo)
                                                                                                              else (ModoJogo (Jogoo (x, y) l Este), jogo)
                                                                                   | otherwise = if (x + 50,y - 50) `notElem` l && (x + 50,y - 100) `elem` l then (ModoJogo (Jogoo (x + 50, y - 50) l Este), jogo)
                                                                                                  else (ModoJogo (Jogoo (x + 50, y) l Este), jogo)
event _ w = w

time :: Float -> World -> World
time _ w = w

caixascords :: World
caixascords = (Controlador Jogar, Jogoo (-250, -100) [(-250,-150),(-200,-150),(-150,-150),(-100,-150),(-50,-150),(0,-150),(50,-150),(100,-150),(150,-150),(200,-150),(250,-150),(-150,-100),(-150,0),(0,-100),(50,-100),(250,-100),(250,-50)] Este)

main :: IO ()
main = do
  playerL <- loadBMP "playerL.bmp"
  playerR <- loadBMP "playerR.bmp"
  caixa <- loadBMP "Caixa.bmp"
  logo <- loadBMP "Logo.bmp"
  play window (greyN 0.263) fr caixascords (draw logo caixa playerL playerR) event time