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

data Opcoes = Jogar
            | Sair

data Menu = Controlador Opcoes
          | ModoJogo Jogoo
          | VenceuJogo

type World = (Menu, Jogo)

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
draw logo caixa playerL playerR (ModoJogo (Jogo (x, y) l d), jogo) = (if d == Oeste then [Translate i j $ scale 0.1 0.1 playerL] else [Translate i j $ scale 0.1 0.1 playerR]) ++ [translate (-212) (154) $ scale 0.3 0.3 logo]
  where
    i = fromIntegral x
    j = fromIntegral y

event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo) = (ModoJogo jogo, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo) = undefined
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo) = (Controlador Jogar, jogo)
event _ (ModoJogo (Jogo m ((x,y) d c), jogo) = (VenceuJogo, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (Jogo m ((x,y) d c)), (Jogo m ((x,y) d c))) = (ModoJogo (Jogo m (trepa ((x,y) d c) m), jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo (Jogo m ((x,y) d c)), (Jogo m ((x,y) d c))) = (ModoJogo (interageCaixa (Jogo m ((x,y) d c) m)), jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (Jogo m ((x,y) d c)), (Jogo m ((x,y) d c))) = (ModoJogo (Jogo m (andaEsqJogador ((x,y) d c) m), jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (Jogo m ((x,y) d c)), (Jogo m ((x,y) d c))) = (ModoJogo (Jogo m (andaDirJogador ((x,y) d c) m), jogo)
event _ w = w

time :: Float -> World -> World
time _ w = w

main :: IO ()
main = do
  playerL <- loadBMP "playerL.bmp"
  playerR <- loadBMP "playerR.bmp"
  caixa <- loadBMP "Caixa.bmp"
  logo <- loadBMP "Logo.bmp"
  bloco <- loadBMP "Bloco.bmp"
  porta <- loadBMP "Porta.bmp"
  play window (greyN 0.263) fr (draw logo caixa playerL playerR) event time