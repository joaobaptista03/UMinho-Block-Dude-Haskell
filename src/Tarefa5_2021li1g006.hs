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
import System.Exit
import Tarefa1_2021li1g006
import Tarefa2_2021li1g006
import Tarefa3_2021li1g006
import Tarefa4_2021li1g006
import LI12122

-- | Opções do menu: Play, Instructions e Exit.
data Options = Play 
            |  Instructions
            |  Exit

-- | Todos os estados possíveis seguidos de um Float (O float resume se no scale do jogo inteiro. Graças a este float é possível aumentar ou diminuir o scale do jogo em relação à janela mesmo dentro do jogo): Controller seguido do data Options (Menu e a opção selecionada), Gamemode seguido do Jogo com a informação dos segundos decorridos guardada (Modo de Jogo), Instructionss (Página das instruções), Win (Quando o player ganha)
data Status = Controller Options Float
            | GameMode Jogo Float Int Float
            | Win Int Float

-- | Janela do jogo, que neste caso é Fullscreen.
window :: Display
window = FullScreen

-- | Cor de fundo.
background :: Color
background = greyN 0.263

-- | Frames por segundo.
fr :: Int
fr = 60

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

-- | draw é a função que desenha tudo na janela: Menu quando as várias opções estão selecionadas, Modo de Jogo (se o mapa for válido pela T1), Estado de Venceu e Página de Instruções.
draw :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Status -> IO Picture
draw logo _ _ _ _ _ _ _ _ win _ _ (Win a b) = return $ scale b b $ Pictures [
                                                                   Translate 20 (-70) $ Color (dark green) $ scale 1.2 1.2 win
                                                                 , translate (-190) (-400) $ color white $ scale 0.2 0.2 $ text "Press ENTER to return to Menu"
                                                                 , translate (-740) 400 $ scale 1 1 logo
                                                                 , color white $ translate 150 385 $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                 , color white $ translate (-320) 210 $ scale 0.5 0.5 $ text $ "You took " ++ show a ++ " seconds!"
                                                                  ]
draw logo _ _ _ _ _ _ _ _ _ mar joaopedro (Controller Play a) = return $ scale a a $ Pictures [
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
draw logo _ _ _ _ _ _ _ _ _ mar joaopedro (Controller Instructions a) = return $ scale a a $ Pictures [
                                                                            Translate (-300) (-100) $ makeButton "Play"
                                                                          , Translate (-300) 0 $ Color (makeColor 0 0 256 1) instructionsButton
                                                                          , Translate (-300) (-300) $ makeButton "Exit"
                                                                          , color white $ translate (-150) (-60) $ scale 0.2 0.2 $ text "Press ' r ' to restart"
                                                                          , color white $ translate (-150) (-95) $ scale 0.2 0.2 $ text "Press ' m ' to go back to menu"
                                                                          , color white $ translate (-150) (-130) $ scale 0.2 0.2 $ text "Press ' w ' or Arrow Up to climb"
                                                                          , color white $ translate (-150) (-165) $ scale 0.2 0.2 $ text "Press ' a ' or Arrow Left to walk left"
                                                                          , color white $ translate (-150) (-200) $ scale 0.2 0.2 $ text "Press ' s ' or Arrow Down to carry a box"
                                                                          , color white $ translate (-150) (-235) $ scale 0.2 0.2 $ text "Press ' d ' or Arrow Right to walk right"
                                                                          , color white $ translate (-150) (-270) $ scale 0.2 0.2 $ text "Press ' - ' or ' + ' to scale the game down or up"
                                                                          , translate 0 300 $ scale 1 1 logo
                                                                          , color white $ translate (-350) (-520) $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                          , translate 700 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 $ rectangleSolid 360 360
                                                                          , translate (-700) 300 joaopedro
                                                                          , translate 700 300 mar
                                                                          ]
draw logo _ _ _ _ _ _ _ _  _ mar joaopedro (Controller Exit a) = return $ scale a a $ Pictures [
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
draw logo block box door1 door2 playerL playerR playerLC playerRC _ _ _ (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return $ if validaPotencialMapa (desconstroiMapa m) then scale a a $ pictures [
                                                                                                translate (-550) 130 $ paraGloss block box door1 door2 (desconstroiMapa m) (GameMode (Jogo m (Jogador (x,y) d c)) n l a)
                                                                                              , translate (-550) 130 $ Pictures [
                                                                                                                                if d == Oeste then Translate (64 * fromIntegral x) ((-64) * fromIntegral y) $ scale 2 2 (if c then playerLC else playerL) else Translate (64 * fromIntegral x) ((-64) * fromIntegral y) $ scale 2 2 (if c then playerRC else playerR)
                                                                                                                              , if c then translate (64 * fromIntegral x) ((-64) * fromIntegral (y-1)) $ scale 2 2 box else Blank
                                                                                                                                ]
                                                                                              , color white $ translate 150 385 $ scale 0.2 0.2 $ text "Copyright 2022 - Joao Pedro Baptista & Mariana Pinto"
                                                                                              , translate (-740) 400 $ scale 1 1 logo
                                                                                              , color white $ translate (-400) 200 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-401) 200 $ scale 0.5 0.5 $ text (show l)
                                                                                              , color white $ translate (-399) 200 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-400) 201 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-400) 199 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-401) 201 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-401) 199 $ scale 0.5 0.5 $ text (show l)
                                                                                              , color white $ translate (-399) 199 $ scale 0.5 0.5 $ text (show l) 
                                                                                              , color white $ translate (-399) 201 $ scale 0.5 0.5 $ text (show l)
                                                                                              , color white $ translate 349 200 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 351 200 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 349 199 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 351 199 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 349 201 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 351 201 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 350 200 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 350 201 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                              , color white $ translate 350 199 $ scale 0.5 0.5 $ text (show (round(n)))
                                                                                               ] else undefined

-- | paraGloss traduz as coordenadas da fase 1 para as coordenadas do Gloss e também desenha as peças do jogo, sendo uma função auxiliar da função draw, quando estamos no Modo de Jogo. Para além disso, guarda a informação do Modo de Jogo e alterna a cor da porta de 500 em 500 milissegundos.
paraGloss :: Picture -> Picture -> Picture -> Picture -> [(Peca, Coordenadas)] -> Status -> Picture
paraGloss _ _ _ _ [] _ = Blank
paraGloss block box door1 door2 ((p, (x,y)):t) (GameMode (Jogo m (Jogador (_,_) d c)) n l a) = pictures 
                                                      [
                                                        translate i j $ scale 2 2 (if p == Bloco then block else if p == Caixa then box else if mod (round (n*1000)) 1000 < 500 then door1 else door2)
                                                      , paraGloss block box door1 door2 t (GameMode (Jogo m (Jogador (x,y) d c)) n l a)
                                                      ]

    where
      i = 64 * fromIntegral x
      j = (-64) * fromIntegral y

-- | Ponto de partida do jogo, que inclui o mapa1, mapa2 e o tempo de jogo decorrido consoante as variáveis inseridas.
jogoinicial :: Float -> Int -> Float -> Status 
jogoinicial t l a | l == 1 = GameMode (Jogo mapa1 (Jogador (1,9) Este False)) 0 1 a
                  | l == 2 = GameMode (Jogo mapa2 (Jogador (1,9) Este False)) t 2 a
                  | l == 3 = GameMode (Jogo mapa3 (Jogador (1,2) Este False)) t 3 a
                  | otherwise = undefined


-- | A função é responsável por guardar no GameMode o tempo decorrido em segundos (float) para que seja possível alternar a cor da porta de 500 em 500 milissegundos.
time :: Float -> Status -> IO Status
time n (GameMode (Jogo m (Jogador (x,y) d c)) a l b) = return $ GameMode (Jogo m (Jogador (x,y) d c)) (a+n) l b
time _ s = return s

-- | Primeiro mapa do jogo.
mapa1 :: Mapa
mapa1 =         [
                  [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Porta]
                , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                                                                                                                                               ]

-- | Segundo mapa do jogo.
mapa2 :: Mapa
mapa2 =         [
                  [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Bloco, Bloco, Caixa, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Porta]
                , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                                                                                                                                              ]

-- | Terceiro mapa do jogo.
mapa3 :: Mapa
mapa3 =         [
                  [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                , [Bloco, Caixa, Caixa, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Porta]
                , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
                                                                                                                                              ]

-- | A função event transforma cada de input do Jogador na sua respetiva ação/movimento e, no fim do jogo, escreve no ficheiro "Wins.txt" os tempos que o jogador demorou para resolver o puzzle.

-- | Por exemplo:

-- | Se o jogador clicar no "a" ou no "d", anda, respetivamente, para a esquerda ou direita.
event :: Event -> Status -> IO Status
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controller Play a) = return $ jogoinicial 0 1 a
event (EventKey (Char 'r') Down _ _) (GameMode _ _ l a) = return $ jogoinicial 0 1 a
event (EventKey (Char 'm') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return (Controller Play a)
event (EventKey (Char 'm') Down _ _) (Controller Play a) = return (Controller Play a)
event (EventKey (Char 'm') Down _ _) (Controller Instructions a) = return (Controller Play a)
event (EventKey (Char 'm') Down _ _) (Controller Exit a) = return (Controller Play a)
event (EventKey (Char 'm') Down _ _) (Win a b) = return (Controller Play b)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Play a) = return $ Controller Exit a
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Play a) = return $ Controller Instructions a
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Exit a) = return $ Controller Instructions a
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Exit a) = return $ Controller Play a
event (EventKey (SpecialKey KeyUp) Down _ _) (Controller Instructions a) = return $ Controller Play a
event (EventKey (SpecialKey KeyDown) Down _ _) (Controller Instructions a) = return $ Controller Exit a
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controller Exit a) = exitSuccess
event (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
event (EventKey (SpecialKey KeyEnter) Down _ _) (Win a b) = return $ Controller Play b
event (EventKey (SpecialKey KeyUp) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (trepa (Jogador (x,y) d c) m)),cordy (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                              return $ Win (round(n)) a
                                                                                                                                                                                                                                                      else return $ jogoinicial n (l+1) a
                                                                                           | otherwise = return $ GameMode (Jogo m (trepa (Jogador (x,y) d c) m)) n l a
event (EventKey (SpecialKey KeyDown) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return $ GameMode (interageCaixa (Jogo m (Jogador (x,y) d c))) n l a
event (EventKey (SpecialKey KeyLeft) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (andaEsqJogador (Jogador (x,y) d c) m)),cordy (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                                         return $ Win (round(n)) a
                                                                                                                                                                                                                                                                  else return $ jogoinicial n (l+1) a
                                                                                             | otherwise = return $ GameMode (Jogo m (andaEsqJogador (Jogador (x,y) d c) m)) n l a
event (EventKey (SpecialKey KeyRight) Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (andaDirJogador (Jogador (x,y) d c) m)),cordy (Jogo m (andaDirJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                                          return $ Win (round(n)) a
                                                                                                                                                                                                                                                                  else return $ jogoinicial n (l+1) a
                                                                                              | otherwise = return $ GameMode (Jogo m (andaDirJogador (Jogador (x,y) d c) m)) n l a
event (EventKey (Char 'w') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (trepa (Jogador (x,y) d c) m)),cordy (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                      return $ Win (round(n)) a
                                                                                                                                                                                                                                              else return $ jogoinicial n (l+1) a
                                                                                   | otherwise = return $ GameMode (Jogo m (trepa (Jogador (x,y) d c) m)) n l a
event (EventKey (Char 's') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return $ GameMode (interageCaixa (Jogo m (Jogador (x,y) d c))) n l a
event (EventKey (Char 'a') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (andaEsqJogador (Jogador (x,y) d c) m)),cordy (Jogo m (andaEsqJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                               return $ Win (round(n)) a
                                                                                                                                                                                                                                                        else return $ jogoinicial n (l+1) a
                                                                                   | otherwise = return $ GameMode (Jogo m (andaEsqJogador (Jogador (x,y) d c) m)) n l a
event (EventKey (Char 'd') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) | snd (saberPorta (desconstroiMapa m)) == (cordx (Jogo m (andaDirJogador (Jogador (x,y) d c) m)),cordy (Jogo m (andaDirJogador (Jogador (x,y) d c) m))) = if l == 3 then do appendFile "Wins.txt" ("\n" ++ "You took " ++ show n ++ " Seconds!")
                                                                                                                                                                                                                                                               return $ Win (round(n)) a
                                                                                                                                                                                                                                                       else return $ jogoinicial n (l+1) a
                                                                                   | otherwise = return $ GameMode (Jogo m (andaDirJogador (Jogador (x,y) d c) m)) n l a
event (EventKey (Char '-') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return (GameMode (Jogo m (Jogador (x,y) d c)) n l (a-0.05))
event (EventKey (Char '-') Down _ _) (Controller Play a) = return (Controller Play (a-0.05))
event (EventKey (Char '-') Down _ _) (Controller Instructions a) = return (Controller Instructions (a-0.05))
event (EventKey (Char '-') Down _ _) (Controller Exit a) = return (Controller Exit (a-0.05))
event (EventKey (Char '-') Down _ _) (Win a b) = return (Win a (b-0.05))
event (EventKey (Char '+') Down _ _) (GameMode (Jogo m (Jogador (x,y) d c)) n l a) = return (GameMode (Jogo m (Jogador (x,y) d c)) n l (a+0.05))
event (EventKey (Char '+') Down _ _) (Controller Play a) = return (Controller Play (a+0.05))
event (EventKey (Char '+') Down _ _) (Controller Instructions a) = return (Controller Instructions (a+0.05))
event (EventKey (Char '+') Down _ _) (Controller Exit a) = return (Controller Exit (a+0.05))
event (EventKey (Char '+') Down _ _) (Win a b) = return (Win a (b+0.05))
event _ w = return w


-- | Esta função retorna a coordenada x do player do jogo dado.
cordx :: Jogo -> Int
cordx (Jogo m (Jogador (x,y) d c)) = x

-- | Esta função retorna a coordenada y do player do jogo dado.
cordy :: Jogo -> Int
cordy (Jogo m (Jogador (x,y) d c)) = y

-- | Estado inicial do programa quando ele é aberto, que neste caso é o menu com o botão Play selecionado.
estado :: Status
estado = Controller Play 0.75

-- | Função principal que junta todas as funções atrás mencionadas, carregando todas as imagens para produzir a janela com o jogo desenhado, sendo ele atualizado consoante os inputs que o utilizador dá.
main :: IO ()
main = do

  joaopedro <- loadBMP "imgs/joaopedro.bmp"
  mariana <- loadBMP "imgs/mariana.bmp"
  win <- loadBMP "imgs/YouWin.bmp"
  playerRC <- loadBMP "imgs/playerRC.bmp"
  playerLC <- loadBMP "imgs/playerLC.bmp"
  playerL <- loadBMP "imgs/playerL.bmp"
  playerR <- loadBMP "imgs/playerR.bmp"
  box <- loadBMP "imgs/box.bmp"
  logo <- loadBMP "imgs/Logo.bmp"
  block <- loadBMP "imgs/block.bmp"
  door1 <- loadBMP "imgs/door1.bmp"
  door2 <- loadBMP "imgs/door2.bmp"

  playIO
    window
    background
    fr
    estado
    (draw logo block box door1 door2 playerL playerR playerLC playerRC win joaopedro mariana)
    event
    time