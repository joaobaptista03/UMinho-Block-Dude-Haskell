{- |
Module      : Tarefa5_2021li1g006
Description : Aplicação gráfica completa
Copyright   : João Pedro Mota Baptista <a100705@alunos.uminho.pt>;
            : Mariana Pinto <a100756@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Tarefa5_2021li1g006 where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tarefa1_2021li1g006
import Tarefa2_2021li1g006
import Tarefa3_2021li1g006
import Tarefa4_2021li1g006
import LI12122

-- | Opções do mnu: Play, Instructions e Exit.
data Options = Play
            |  Instructions
            |  Exit

-- | Todos os estados possíveis: Controller seguido do data Options (Menu e a opção selecionada), Gamemode seguido do Jogo (Modo de Jogo), Instructionss (Página das instruções), Win (Quando o player ganha)
data Status = Controller Options
            | GameMode Jogo
            | Instructionss
            | Win

-- | Janela do jogo, que neste caso é Fullscreen.
window :: Display
window = FullScreen

-- | Cor de fundo.
background :: Color
background = greyN 0.263

-- | Frames por segundo.
fr :: Int
fr = 30

-- | Esta função cria um botão consoante a string que lhe é dada para criar as opções do menu.

-- | Por exemplo:

-- | makeButton Play produz a picture do botão play do menu.
makeButton :: String -> Picture 
makeButton texto = pictures
    [ translate 0 50 $ color black $ rectangleSolid 204 54
    , translate 0 50 $ color (greyN 0.8) $ rectangleSolid 200 50
    , translate (-29) 38 $ scale 0.26 0.26 $ text texto]

-- | instructionsButton é especificamente o botão do menu de Instrutcions. Não dá para usar a função makeButton pois "Instructions" não ficava centrado no botão, tendo de criá-lo à parte.
instructionsButton :: Picture 
instructionsButton = pictures
    [ translate 0 (-150) $ color black $ rectangleSolid 204 54
    , translate 0 (-150) $ color (greyN 0.8) $ rectangleSolid 200 50
    , translate (-85) (-162) $ scale 0.26 0.26 $ text "Instructions"]

-- | draw é a função que desenha tudo na janela: Menu quando as várias opções estão selecionadas, Modo de Jogo, Estado de Venceu e Página de Instruções.
draw :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Status -> Picture
draw logo block box door playerL playerR win mar joaopedro Win = Pictures [
                                                                   Translate 20 (-40) $ Color (dark green) $ scale 2 2 win
                                                                 , translate (-190) (-400) $ color white $ scale 0.2 0.2 $ text "Press ENTER to return to Menu"
                                                                 , translate (-740) 400 $ scale 1 1 logo
                                                                  ]
draw logo block box door playerL playerR win mar joaopedro (Controller Play) = Pictures [
                                                                            Color (bright magenta) $ Translate 0 (-100) $ makeButton "Play"
                                                                          , instructionsButton
                                                                          , Translate 0 (-300) $ makeButton "Exit"
                                                                          , translate 0 300 $ scale 1 1 logo
                                                                          , color white $ translate (-350) (-520) $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                          , translate 700 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 joaopedro
                                                                          , translate 700 300 mar
                                                                           ]
draw logo block box door playerL playerR win mar joaopedro (Controller Instructions) = Pictures [
                                                                            Translate (-300) (-100) $ makeButton "Play"
                                                                          , Translate (-300) 0 $ Color (makeColor 0 0 256 1) instructionsButton
                                                                          , Translate (-300) (-300) $ makeButton "Exit"
                                                                          , color white $ translate (-150) (-70) $ scale 0.2 0.2 $ text "Press ' r ' to restart"
                                                                          , color white $ translate (-150) (-105) $ scale 0.2 0.2 $ text "Press ' m ' to go back to menu"
                                                                          , color white $ translate (-150) (-140) $ scale 0.2 0.2 $ text "Press ' w ' or keyUp to climb"
                                                                          , color white $ translate (-150) (-175) $ scale 0.2 0.2 $ text "Press ' a ' or keyLeft to walk left"
                                                                          , color white $ translate (-150) (-210) $ scale 0.2 0.2 $ text "Press ' s ' or keyDown to carry a box"
                                                                          , color white $ translate (-150) (-245) $ scale 0.2 0.2 $ text "Press ' d ' or keyRight to walk right"
                                                                          , translate 0 300 $ scale 1 1 logo
                                                                          , color white $ translate (-350) (-520) $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                          , translate 700 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 joaopedro
                                                                          , translate 700 300 mar
                                                                          ]
draw logo block box door playerL playerR win mar joaopedro (Controller Exit) = Pictures [
                                                                           Translate 0 (-100) $ makeButton "Play"
                                                                         , instructionsButton
                                                                         , Color (makeColor 0 212 255 1) $ Translate 0 (-300) $ makeButton "Exit"
                                                                         , translate 0 300 $ scale 1 1 logo
                                                                         , color white $ translate (-350) (-520) $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                         , translate 700 300 $ rectangleSolid 360 360
                                                                         , translate (-700) 300 $ rectangleSolid 360 360
                                                                         , translate (-700) 300 joaopedro
                                                                         , translate 700 300 mar
                                                                          ]
draw logo block box door playerL playerR win mar joaopedro (GameMode (Jogo m (Jogador (x,y) d c))) = pictures [
                                                                                                translate (-550) 130 $ paraGloss block box door (desconstroiMapa m)
                                                                                              , translate (-550) 130 $ Pictures [
                                                                                                                                if d == Oeste then Translate (64 * fromIntegral x) ((-64) * fromIntegral y) $ scale 2 2 playerL else Translate (64 * fromIntegral x) ((-64) * fromIntegral y) $ scale 2 2 playerR
                                                                                                                              , if c then translate (64 * fromIntegral x) ((-64) * fromIntegral (y-1)) $ scale 2 2 box else Blank
                                                                                                                                ]
                                                                                              , color white $ translate 150 385 $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                                              , translate (-740) 400 $ scale 1 1 logo
                                                                                               ]

-- | paraGloss traduz as coordenadas da fase 1 para as coordenadas do Gloss e também desenha as peças do jogo, sendo uma função auxiliar da função draw, quando estamos no Modo de Jogo.
paraGloss :: Picture -> Picture -> Picture -> [(Peca, Coordenadas)] -> Picture
paraGloss _ _ _ [] = Blank
paraGloss block box door ((p, (x,y)):t) = pictures 
                                                      [
                                                        translate i j $ scale 2 2 (if p == Bloco then block else if p == Caixa then box else door)
                                                      , paraGloss block box door t
                                                      ]
    where
      i = 64 * fromIntegral x
      j = (-64) * fromIntegral y

-- | Ponto de partida do jogo, que inclui o mapa1.
jogoinicial :: Jogo 
jogoinicial = Jogo mapa1 (Jogador (1,9) Este False)

-- | A função termina o jogo quando o player chega à porta.
finishgame :: Float -> Status -> Status
finishgame _ (GameMode (Jogo m (Jogador (x,y) d c))) | snd (saberPorta (desconstroiMapa m)) == (x,y) = Win
                                                     | otherwise = GameMode (Jogo m (Jogador (x,y) d c))
finishgame _ o = o

-- | Primeiro mapa do jogo.
mapa1 :: Mapa
mapa1 =         [
                  [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Porta]
                , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                                                                                                                                              ]

-- | A função event transforma cada de input do Jogador na sua respetiva ação/movimento.

-- |Por exemplo:

-- | Se o jogador clicar no "a" ou no "d", anda, respetivamente, para a esquerda ou direita.
event :: Event -> Status -> Status
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controller Play) = GameMode jogoinicial
event (EventKey (Char 'r') Down _ _) (GameMode _) = GameMode jogoinicial
event (EventKey (Char 'm') Down _ _) _ = Controller Play
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Play) = Controller Exit
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Play) = Controller Instructions
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Exit) = Controller Instructions
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Exit) = Controller Play
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Instructions) = Controller Play
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Instructions) = Controller Exit
event (EventKey (SpecialKey KeyEnter) Down _ _) Instructionss = Controller Play
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controller Exit) = undefined
event (EventKey (SpecialKey KeyEnter) Down _ _) Win = Controller Play
event (EventKey (SpecialKey KeyUp) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (trepa (Jogador (x,y) d c) m))
event (EventKey (SpecialKey KeyDown) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (interageCaixa (Jogo m (Jogador (x,y) d c)))
event (EventKey (SpecialKey KeyLeft) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))
event (EventKey (SpecialKey KeyRight) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (andaDirJogador (Jogador (x,y) d c) m))
event (EventKey (Char 'w') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (trepa (Jogador (x,y) d c) m))
event (EventKey (Char 's') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (interageCaixa (Jogo m (Jogador (x,y) d c)))
event (EventKey (Char 'a') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))
event (EventKey (Char 'd') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c))) = GameMode (Jogo m (andaDirJogador (Jogador (x,y) d c) m))
event _ w = w

-- | Estado inicial do programa quando ele é aberto, que neste caso é o menu com o botão Play selecionado.
estado :: Status
estado = Controller Play

-- | Função principal que junta todas as funções atrás mencionadas, carregando todas as imagens para produzir a janela com o jogo desenhado, sendo ele atualizado consoante os inputs que o utilizador dá.
main :: IO ()
main = do

  joaopedro <- loadBMP "imgs/joaopedro.bmp"
  mariana <- loadBMP "imgs/mariana.bmp"
  win <- loadBMP "imgs/YouWin.bmp"
  playerL <- loadBMP "imgs/playerL.bmp"
  playerR <- loadBMP "imgs/playerR.bmp"
  box <- loadBMP "imgs/box.bmp"
  logo <- loadBMP "imgs/Logo.bmp"
  block <- loadBMP "imgs/block.bmp"
  door <- loadBMP "imgs/door.bmp"

  play 
    FullScreen
    background
    fr
    estado
    (draw logo block box door playerL playerR win mariana joaopedro)
    event
    finishgame