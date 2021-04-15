module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import Score
import Som

type Position = (Float, Float)

window :: Display
window = InWindow "GuitarGame" (550, 500) (500, 150)

background :: Color
background = black

data GuitarGame = Game { noteLoc1 :: (Float, Float)
                       , noteVel1 :: (Float, Float)
                       , noteLoc2 :: (Float, Float)
                       , noteVel2 :: (Float, Float)
                       , noteLoc3 :: (Float, Float)
                       , noteVel3 :: (Float, Float)
                       , noteLoc4 :: (Float, Float)
                       , noteVel4 :: (Float, Float)
                       , noteLoc5 :: (Float, Float)
                       , noteVel5 :: (Float, Float)
                       , freat1 :: Float
                       , freat2 :: Float
                       , freat3 :: Float
                       , freat4 :: Float
                       , freat5 :: Float
                       , score1 :: String
                       , score2 :: String
                       } deriving Show

initialState :: GuitarGame
initialState = Game { noteLoc1 = (-200, 200)
                    , noteVel1 = (0, -100)
                    , noteLoc2 = (-125, 150)
                    , noteVel2 = (0, -100)
                    , noteLoc3 = (-50, 120)
                    , noteVel3 = (0, -100)
                    , noteLoc4 = (25, 170)
                    , noteVel4 = (0, -100)
                    , noteLoc5 = (100, 110)
                    , noteVel5 = (0, -100)
                    , freat1 = (-200)
                    , freat2 = (-125)
                    , freat3 = (-50)
                    , freat4 = 25
                    , freat5 = 100
                    , score1 = "0"
                    , score2 = "50"
                    }

render :: GuitarGame -> Picture
render game = Pictures [lineGuitars,notes,freats,walls,bases,scores]
               where
                note1 = uncurry translate (noteLoc1 game) $ color green $ circleSolid 12
                note2 = uncurry translate (noteLoc2 game) $ color red $ circleSolid 12
                note3 = uncurry translate (noteLoc3 game) $ color yellow $ circleSolid 12
                note4 = uncurry translate (noteLoc4 game) $ color blue $ circleSolid 12
                note5 = uncurry translate (noteLoc5 game) $ color orange $ circleSolid 12

                notes = Pictures [note1,note2,note3,note4,note5]

                score offset col texto = translate offset col $ scale (0.3) (0.3) $ color (greyN 0.5) $ text texto
                scores = Pictures [score (200) (-200) (score1 game), score (200) (-150) (score2 game)]

                freat :: Float -> Color -> Picture
                freat offset col = translate offset (-125) $ color col $ circle 12
                freats = Pictures [freat (-200) green, freat (-125) red, freat (-50) yellow, freat (25) blue, freat (100) orange]

                wall :: Float -> Picture
                wall offset = translate offset 0 $ color wallColor $ rectangleSolid 6 480
                wallColor = greyN 0.5
                walls = Pictures [wall (-270), wall (170)]

                base :: Float -> Picture
                base col = translate (-50) col $ color baseColor $ rectangleSolid 446 6
                baseColor = greyN 0.5
                bases = Pictures [ base (-240)]

                lineGuitar :: Float -> Picture
                lineGuitar offset = translate offset 0 $ color lineColor $ rectangleSolid 1 480
                lineColor = greyN 0.5
                lineGuitars = Pictures [ lineGuitar (-200), lineGuitar (-125), lineGuitar (-50), lineGuitar (25), lineGuitar (100)]

moveNote :: Float -> GuitarGame -> GuitarGame
moveNote seconds game = game { noteLoc1 = (x1', y1'),
                               noteLoc2 = (x2', y2'),
                               noteLoc3 = (x3', y3'),
                               noteLoc4 = (x4', y4'),
                               noteLoc5 = (x5', y5')  }
            where
              (x1, y1) = noteLoc1 game
              (vx1, vy1) = noteVel1 game
              (x2, y2) = noteLoc2 game
              (vx2, vy2) = noteVel2 game
              (x3, y3) = noteLoc3 game
              (vx3, vy3) = noteVel3 game
              (x4, y4) = noteLoc4 game
              (vx4, vy4) = noteVel4 game
              (x5, y5) = noteLoc5 game
              (vx5, vy5) = noteVel5 game
          -- new state
              x1' = x1
              y1' = y1 + vy1 * seconds

              x2' = x2
              y2' = y2 + vy2 * seconds

              x3' = x3
              y3' = y3 + vy3 * seconds

              x4' = x4
              y4' = y4 + vy4 * seconds

              x5' = x5
              y5' = y5 + vy5 * seconds

basesCollision :: Position -> Bool
basesCollision (_, y) = bottomCollision
        where
            bottomCollision = y - 6 <=  fromIntegral (-234)

basesBounce :: GuitarGame -> GuitarGame
basesBounce game = game { noteLoc1 = (x1', y1'),
                          noteLoc2 = (x2', y2'),
                          noteLoc3 = (x3', y3'),
                          noteLoc4 = (x4', y4'),
                          noteLoc5 = (x5', y5'),
                          score2 = str'}
                  where
                    str = map digitToInt (score2 game)
                    a = (somarPontuacao str ) - 1
                    str' = if basesCollision (noteLoc1 game ) || basesCollision (noteLoc2 game ) || basesCollision (noteLoc3 game ) || basesCollision (noteLoc4 game ) || basesCollision (noteLoc5 game )   then map intToDigit (adicionar a) else map intToDigit str

                    (x1', y1) = noteLoc1 game
                    (x2', y2) = noteLoc2 game
                    (x3', y3) = noteLoc3 game
                    (x4', y4) = noteLoc4 game
                    (x5', y5) = noteLoc5 game
                    y1' = if basesCollision (noteLoc1 game )  then (225)  else y1
                    y2' = if basesCollision (noteLoc2 game )  then (225)  else y2
                    y3' = if basesCollision (noteLoc3 game )  then (225)  else y3
                    y4' = if basesCollision (noteLoc4 game )  then (225)  else y4
                    y5' = if basesCollision (noteLoc5 game )  then (225)  else y5


match :: Position -> Bool
match (_, y) = matchFreats
                where
                  matchFreats = y <= (-113) && y >= (-131)

-- | Respond to key events.
handleKeys :: Event -> GuitarGame -> GuitarGame
handleKeys (EventKey (Char 'a') _ _ _) game = game { noteLoc1 = (x1',y1')
                                                    ,score1 = str'}
                                      where
                                        str = map digitToInt (score1 game)
                                        a = (somarPontuacao str ) + 1
                                        str' = if match (noteLoc1 game ) then map intToDigit (adicionar a) else map intToDigit str
                                        (x1',y1) = noteLoc1 game
                                        y1' = if match (noteLoc1 game ) then 225 else y1

handleKeys (EventKey (Char 's') _ _ _) game = game { noteLoc2 = (x2',y2')
                                                     ,score1 = str'  }
                                      where
                                        str = map digitToInt (score1 game)
                                        a = (somarPontuacao str ) + 1
                                        str' = if match (noteLoc2 game ) then map intToDigit (adicionar a) else map intToDigit str
                                        (x2',y2) = noteLoc2 game
                                        y2' = if match (noteLoc2 game ) then 225 else y2

handleKeys (EventKey (Char 'd') _ _ _) game = game { noteLoc3 = (x3',y3')
                                                     ,score1 = str' }
                                      where
                                        str = map digitToInt (score1 game)
                                        a = (somarPontuacao str ) + 1
                                        str' = if match (noteLoc3 game ) then map intToDigit (adicionar a) else map intToDigit str
                                        (x3',y3) = noteLoc3 game
                                        y3' = if match (noteLoc3 game ) then 225 else y3
handleKeys (EventKey (Char 'f') _ _ _) game = game { noteLoc4 = (x4',y4')
                                                    ,score1 = str'  }
                                      where
                                        str = map digitToInt (score1 game)
                                        a = (somarPontuacao str ) + 1
                                        str' = if match (noteLoc4 game ) then map intToDigit (adicionar a) else map intToDigit str
                                        (x4',y4) = noteLoc4 game
                                        y4' = if match (noteLoc4 game ) then 225 else y4
handleKeys (EventKey (Char 'g') _ _ _) game = game { noteLoc5 = (x5',y5')
                                                    ,score1 = str'  }
                                      where
                                        str = map digitToInt (score1 game)
                                        a = (somarPontuacao str ) + 1
                                        str' = if match (noteLoc5 game ) then map intToDigit (adicionar a) else map intToDigit str
                                        (x5',y5) = noteLoc5 game
                                        y5' = if match (noteLoc5 game ) then 225 else y5
handleKeys _ game = game


--frames per seconds
fps :: Int
fps = 60

update :: Float -> GuitarGame -> GuitarGame
update seconds = basesBounce . moveNote seconds

main :: IO ()
main = do
        notasMusicais 1
        play window background fps initialState render handleKeys update
