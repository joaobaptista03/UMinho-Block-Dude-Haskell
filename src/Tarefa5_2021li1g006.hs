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

data Jogo = Jogo (Int, Int) [(Int, Int)]

data Opcoes = Jogar
            | Sair

data Menu = Controlador Opcoes
          | ModoJogo Jogo
          | VenceuJogo

type World = (Menu, Jogo)

window :: Display
window = InWindow "Jumper Dude by Joao Baptista e Mariana Pinto" (600, 400) (760,340)

fr :: Int
fr = 144

playlogo :: String -> Picture 
playlogo texto = pictures
    [ translate 0 50 $ color black $ rectangleSolid 204 54
    , translate 0 50 $ color (greyN 0.8) $ rectangleSolid 200 50
    , translate (-29) 38 $ scale 0.26 0.26 $ text texto]



draw :: Picture -> World -> Picture
draw pic (VenceuJogo, jogo) = Pictures [Translate (-280) (-40) $ Color (dark green) $ Text "You Win!", translate (-230) (150) $ scale 0.3 0.3 pic]
draw pic (Controlador Jogar, jogo) = Pictures [Color blue $ Translate (-110) (-100) $ playlogo "Play", Translate 110 (-100) $ playlogo "Exit", translate 0 100 $ scale 0.5 0.5 pic]
draw pic (Controlador Sair, jogo) = Pictures [Translate (-110) (-100) $ playlogo "Play", Color magenta $ Translate 110 (-100) $ playlogo "Exit", translate 0 100 $ scale 0.5 0.5 pic]
draw pic (ModoJogo (Jogo (x, y) l), jogo) = Pictures [translate (-230) (150) $ scale 0.3 0.3 pic]

drawOption :: String -> Picture
drawOption option = Translate (-60) 20 $ Scale 0.5 0.5 $ Text option

drawSmallCircles :: (Int, Int) -> Picture
drawSmallCircles (x, y) = Translate i j $ Circle 5
    where
      i = fromIntegral x
      j = fromIntegral y

engine :: (Int, Int) -> [(Int, Int)] -> Jogo
engine p l = Jogo p (filter (p/=) l)

event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo) = (ModoJogo jogo, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo) = undefined
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo) = (Controlador Jogar, jogo)
event _ (ModoJogo (Jogo (x, y) []), jogo) = (VenceuJogo, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (Jogo (x, y) l), jogo) = (ModoJogo $ engine (x, y + 50) l, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo (Jogo (x, y) l), jogo) = (ModoJogo $ engine (x, y - 50) l, jogo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (Jogo (x, y) l), jogo) = (ModoJogo $ engine (x - 50, y) l, jogo)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (Jogo (x, y) l), jogo) = (ModoJogo $ engine (x + 50, y) l, jogo)
event _ w = w

time :: Float -> World -> World
time _ w = w

estado :: World
estado = (Controlador Jogar, Jogo (200, 100) [(50, 50), (-250, -100), (-100, -50)])

main :: IO ()
main = do
  logo <- loadBMP "Logo.bmp"
  play window (greyN 0.263) fr estado (draw logo) event time