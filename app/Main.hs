{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)

import CodeWorld

data Direction = U | D | L | R
  deriving Eq

data Coord = Coord Integer Integer
  deriving Eq

data State = State Integer Coord Direction (List Coord)
  deriving Eq

data Tile = Blank | Wall | Ground | Storage | Box
  deriving Eq

initialState :: Integer -> State
initialState 0 = initialStateL0
initialState 1 = initialStateL1
initialState 2 = initialStateL2
initialState 3 = initialStateL3
initialState 4 = initialStateL4
initialState 5 = initialStateL5
initialState 6 = initialStateL6
initialState (-1) = initialStateLm1
initialState n = initialStateL0

maze :: Integer -> (Coord -> Tile)
maze 0 = mazeL0
maze 1 = mazeL1
maze 2 = mazeL2
maze 3 = mazeL3
maze 4 = mazeL4
maze 5 = mazeL5
maze 6 = mazeL6
maze (-1) = mazeLm1
maze n = mazeL0

initialStateL0 :: State
initialStateL0 = State 0 (Coord 0 (-1)) U initialBoxesL0

initialBoxesL0 :: List Coord
initialBoxesL0 =
  Coord 1 0 `Entry`
  Coord 0 0 `Entry`
  Coord (-1) 0 `Entry`
  Coord (-2) 0 `Entry`
  Empty

mazeL0 :: Coord -> Tile
mazeL0 (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | otherwise                = Ground

initialStateL1 :: State
initialStateL1 = State 1 (Coord (-2) 2) R initialBoxesL1

initialBoxesL1 :: List Coord
initialBoxesL1 =
  Coord 0 1 `Entry`
  Coord (-1) 1 `Entry`
  Empty

mazeL1 :: Coord -> Tile
mazeL1 (Coord x y)
  | abs x > 3 || y > 3 || y < -2    = Blank
  | x == 3  && y == 3               = Blank
  | abs x == 3 || y == 3 || y == -2 = Wall
  | x == 2  && y == 2               = Wall
  | x == -1 && y == 0               = Wall
  | (x == 0 || x == 2) && y == 0    = Storage
  | otherwise                       = Ground

initialStateL2 :: State
initialStateL2 = State 2 (Coord 0 0) U initialBoxesL2

initialBoxesL2 :: List Coord
initialBoxesL2 =
  Coord 1 1 `Entry`
  Coord 0 1 `Entry`
  Coord (-1) 1 `Entry`
  Coord 1 0 `Entry`
  Coord (-1) 0 `Entry`
  Coord 1 (-1) `Entry`
  Coord 0 (-1) `Entry`
  Coord (-1) (-1) `Entry`
  Empty

mazeL2 :: Coord -> Tile
mazeL2 (Coord x y)
  | abs x > 3  || abs y > 3  = Blank
  | abs x == 3 || abs y == 3 = Wall
  | y == 0 || y == 1         = Ground
  | x == 0                   = Ground
  | y /= -2 && abs x < 2     = Ground
  | otherwise                = Storage

initialStateL3 :: State
initialStateL3 = State 3 (Coord 0 0) D initialBoxesL3

initialBoxesL3 :: List Coord
initialBoxesL3 =
  Coord 1 0 `Entry`
  Coord (-1) 0 `Entry`
  Coord 2 (-1) `Entry`
  Coord 3 (-1) `Entry`
  Coord (-2) (-1) `Entry`
  Coord (-3) (-1) `Entry`
  Empty

mazeL3 :: Coord -> Tile
mazeL3 (Coord x y)
  | abs x > 5 || y > 2 || y < -3       = Blank
  | x == 0 && (y == -2 || y == -3)     = Blank
  | abs x == 5 || y == 2 || y == -3    = Wall
  | x > -2 && x < 2 && y > -3 && y < 0 = Wall
  | x == 0 && y == 1                   = Wall
  | x > -4 && x < 4 && y == 1          = Storage
  | otherwise                          = Ground

initialStateL4 :: State
initialStateL4 = State 4 (Coord (-2) 0) R initialBoxesL4

initialBoxesL4 :: List Coord
initialBoxesL4 =
  Coord (-2) 3 `Entry`
  Coord (-1) (-1) `Entry`
  Coord (-3) (-4) `Entry`
  Coord (-1) (-4) `Entry`
  Coord 1 1 `Entry`
  Coord 1 3 `Entry`
  Coord 3 0 `Entry`
  Coord 3 1 `Entry`
  Coord 3 2 `Entry`
  Coord 3 3 `Entry`
  Empty

mazeL4 :: Coord -> Tile
mazeL4 (Coord x y)
  | abs x > 5 || y > 5 || y < -6    = Blank
  | (x == -4 || x == -5) && y > -2  = Blank
  | x > 0 && (y == -5 || y == -6)   = Blank
  
  | x > -1 && x < 3 && y == 0           = Storage
  | x > -2 && x < 4 && y == -3          = Storage
  | x == 3 && y > -3 && y < 0           = Storage
  
  | x > -3 && x < 0 && y > 1 && y < 5   = Ground
  | x == -2 && y > -3 && y < 2          = Ground
  | x > 0 && x < 5 && y > -1 && y < 5   = Ground
  | x > -5 && x < 0 && y > -6 && y < -2 = Ground
  | x > -2 && x < 1 && y > -2 && y < 1  = Ground
  | x > 2 && x < 5 && y > -4 && y < -1  = Ground
  | otherwise                           = Wall

initialStateL5 :: State
initialStateL5 = State 5 (Coord (-2) 0) R initialBoxesL5

initialBoxesL5 :: List Coord
initialBoxesL5 =
  Coord 0 2 `Entry`
  Coord 1 2 `Entry`
  Coord 2 2 `Entry`
  Coord 3 2 `Entry`
  Coord 0 1 `Entry`
  Coord 0 (-1) `Entry`
  Coord 0 (-2) `Entry`
  Coord (-1) (-2) `Entry`
  Coord (-2) (-2) `Entry`
  Coord (-3) (-2) `Entry`
  Empty

mazeL5 :: Coord -> Tile
mazeL5 (Coord x y)
  | abs x > 4 || abs y > 4                = Blank
  | abs x == 4 || abs y == 4              = Wall
  | x == 0 && y == 0                      = Wall
  | abs x == 2 && abs y == 2 && x * y < 0 = Wall
  | abs x == 2 && abs y == 1 && x * y < 0 = Wall
  | abs x == 1 && abs y == 2 && x * y < 0 = Wall
  | abs x == 2 || abs y == 2              = Ground
  | abs x > 0 && abs y > 0 && x * y < 0   = Storage
  | abs x == 3 && abs y == 3 && x * y > 0 = Storage
  | otherwise                          = Ground

initialStateL6 :: State
initialStateL6 = State 6 (Coord 0 0) U initialBoxesL6

initialBoxesL6 :: List Coord
initialBoxesL6 =
  Coord 6 1 `Entry`
  Coord (-6) 1 `Entry`
  Coord 1 2 `Entry`
  Coord (-1) 2 `Entry`
  Coord 0 (-3) `Entry`
  Empty

mazeL6 :: Coord -> Tile
mazeL6 (Coord x y)
  | abs x > 9 || abs y > 6               = Blank
  | abs x > 3 && y < -2                  = Blank
  | abs x > 4 && y > 3                   = Blank
  | abs x == 9 || abs y == 6             = Wall
  | abs x > 2 && y < -1                  = Wall
  | abs x > 3 && y > 2                   = Wall
  | y == 0                               = Ground
  | abs x == 3 && y < 4                  = Wall
  | (abs x == 5 || abs x == 7) && y == 1 = Wall
  | x == 0 && (y == -2 || y == -4)       = Wall
  | abs x == 1 && (y == 1 || y == 3)     = Wall
  | y == 5 && (x == 0 || abs x == 3)     = Storage
  | y == 4 && abs x == 1                 = Storage
  | otherwise                            = Ground

initialStateLm1 :: State
initialStateLm1 = State (-1) (Coord 0 0) U initialBoxesLm1

initialBoxesLm1 :: List Coord
initialBoxesLm1 =
  Coord 2 2 `Entry`
  Coord 2 3 `Entry`
  Coord 3 3 `Entry`
  Coord 3 4 `Entry`
  Coord (-2) 2 `Entry`
  Coord (-2) 3 `Entry`
  Coord (-3) 3 `Entry`
  Coord (-3) 4 `Entry`
  Coord 2 (-2) `Entry`
  Coord 2 (-3) `Entry`
  Coord 3 (-3) `Entry`
  Coord 3 (-4) `Entry`
  Coord (-2) (-2) `Entry`
  Coord (-2) (-3) `Entry`
  Coord (-3) (-3) `Entry`
  Coord (-3) (-4) `Entry`
  Empty

mazeLm1 :: Coord -> Tile
mazeLm1 (Coord x y)
  | abs x > 7 || abs y > 5               = Blank
  | abs x == 7 && abs y < 2              = Blank
  | abs x < (abs y) - 3 && abs y > 3     = Blank
  | abs x == 7 || abs y == 5             = Wall
  | abs x == 3 && abs y == 4             = Wall
  | (abs x == 4 || abs x == 5) && y == 0 = Storage
  | abs x < 3 && abs y < 2               = Ground
  | abs x == 3 && y == 0                 = Ground
  | abs x == 5                           = Ground
  | abs x == 6 && abs y > 1              = Ground
  | x == 0 && abs y == 2                 = Ground
  | abs x == (abs y) - 2                 = Storage
  | abs x > 1 && abs y == 4              = Ground
  | abs x == 3 && abs y == 2             = Storage
  | abs x == 4 && abs y == 1             = Ground
  | otherwise                            = Wall

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) = translated (fromIntegral x) (fromIntegral y)

wall, ground, storage, box :: Picture
wall = translated (-0.25) (-0.25) (colored grey (solidRectangle 0.5 0.5)) & translated 0.25 0.25 (colored grey (solidRectangle 0.5 0.5)) & colored (dark grey) (solidRectangle 1 1)
ground = colored (light brown) (solidRectangle 1 1)
storage = colored (translucent green) (solidCircle 0.25) & colored (light brown) (solidRectangle 1 1)
box = colored (dark brown) (thickRectangle 0.2 0.8 0.8) & colored brown (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile n = blank

drawTileAt :: Coord -> Tile -> Picture
drawTileAt (Coord x y) n = atCoord (Coord x y) (drawTile n)

moveCoord :: Direction -> Coord -> Coord
moveCoord U (Coord x y) = Coord x (y + 1)
moveCoord D (Coord x y) = Coord x (y - 1)
moveCoord L (Coord x y) = Coord (x - 1) y
moveCoord R (Coord x y) = Coord (x + 1) y

player :: Direction -> Picture
player U = initialPlayer
player D = rotated pi initialPlayer
player L = rotated (pi / 2) initialPlayer
player R = rotated (-pi / 2) initialPlayer

initialPlayer :: Picture
initialPlayer =
  let
    leftEye = translated (-0.1) 0.14 (colored black (solidCircle 0.03)) & translated (-0.1) 0.1 (colored white (solidCircle 0.08))
    rightEye = translated 0.1 0.14 (colored black (solidCircle 0.03)) & translated 0.1 0.1 (colored white (solidCircle 0.08))
    playerHead = colored black (solidCircle 0.2)
    leftHand = polyline [(-0.1, 0), (-0.3, 0.15), (-0.35, 0.35), (-0.3, 0.35)]
    rightHand = polyline [(0.1, 0), (0.3, 0.15), (0.35, 0.35), (0.3, 0.35)]
    body = polyline [(0, 0), (0, -0.3)]
    leftLeg = polyline [(0, -0.3), (-0.2, -0.4), (-0.2, -0.3)]
    rightLeg = polyline [(0, -0.3), (0.2, -0.4), (0.2, -0.3)]
  in leftEye & rightEye & playerHead & leftHand & rightHand & body & leftLeg & rightLeg

left, right :: Integer
left = -10
right = 10

drawMaze :: Integer -> Coord -> Picture
drawMaze l (Coord x y)
  | y > right = blank
  | x > right = drawMaze l (Coord left (y + 1))
  | otherwise = drawTileAt (Coord x y) (maze l (Coord x y)) & (drawMaze l (Coord (x + 1) y))

data List a = Empty | Entry a (List a)
  deriving Eq
infixr `Entry`

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry x xs) = x & combine xs

appendList :: List a -> List a -> List a
appendList Empty list2 = list2
appendList (Entry x xs) list2 = Entry x (appendList xs list2)

change :: Coord -> Coord -> List Coord -> List Coord
change xOld xNew Empty = Empty
change xOld xNew (Entry x xs) = if x == xOld
    then Entry xNew xs 
    else Entry x (change xOld xNew xs)
    
find :: List Coord -> Coord -> Bool
find xs xToFind = not (allList (mapList not (mapList (== xToFind) xs)))

allList :: List Bool -> Bool
allList Empty = True
allList (Entry False bs) = False
allList (Entry True bs) = allList bs

listElement :: List a -> Integer -> a
listElement list i = help list 0 
    help :: List a -> Integer -> Integer -> a
    help (Entry x Empty) j i = x
    help (Entry x xs) j i 
      | j == i = x
      | otherwise = help xs (j + 1) i

drawBoxes :: Integer -> List Coord -> Picture
drawBoxes l boxes = combine (mapList drawBoxAt boxes
    drawBoxAt boxCoord = (if (maze l boxCoord) == Storage
        then atCoord boxCoord (colored (translucent green) (solidCircle 0.25)) 
        else blank) & 
        atCoord boxCoord (drawTile Box)

data Activity world = Activity world (Event -> world -> world) (world -> Picture)

resetable :: Activity State -> Activity State
resetable (Activity is handle draw) = (Activity is handle' draw
    handle' (KeyPress "Esc") (State l coord d boxes) = initialState l
    handle' (KeyPress "R") (State l coord d boxes) = initialState l
    handle' e s = handle e s

data SSState world = StartScreen | Running world
  deriving Eq

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban") & 
    translated 0 (-7) (lettering "Press space to start")

withStartScreen :: Activity world -> Activity (SSState world)
withStartScreen (Activity initialWorld handle draw) = (Activity initialWorld' handle' draw'
    initialWorld' = StartScreen
    
    handle' (KeyPress " ") StartScreen = Running initialWorld
    handle' e StartScreen = StartScreen
    handle' e (Running w) = Running (handle e w)
    
    draw' StartScreen = startScreen
    draw' (Running w) = draw w

winScreen :: Picture
winScreen = scaled 3 3 (lettering "You Won!") & 
    translated 0 (-7) (lettering "Press L to select another level")

boxesInStorages :: Integer -> List Coord -> Bool
boxesInStorages l boxes = allList (mapList (== Storage) (mapList (maze l) boxes))

isWon :: State -> Bool
isWon (State l coords d boxes)
  | boxesInStorages l boxes = True
  | otherwise               = False

withWin :: Activity State -> Activity State
withWin (Activity initialState handle draw) = (Activity initialState handle' draw'
    handle' e s
      | isWon s   = s
      | otherwise = handle e s
    
    draw' s 
      | isWon s   = winScreen
      | otherwise = draw s

levelSelectScreen :: Picture
levelSelectScreen = translated 0 8 (scaled 0.8 0.8 (lettering "Press H if you need help")) &
    translated 0 1 (scaled 2 2 (lettering "Select Level")) & 
    translated 0 (-1) (lettering "Press key from 0 to 6") & 
    translated 0 (-7) (scaled 0.8 0.8 (lettering "This is the level selection screen")) & 
    translated 0 (-8) (scaled 0.8 0.8 (lettering "Press L to open it again"))

data LevelState world = LevelSelection | Game world
  deriving Eq

withLevels :: Activity State -> Activity (LevelState State)
withLevels (Activity is handle draw) = (Activity is' handle' draw'
    is' = LevelSelection
    
    handle' (KeyPress "0") LevelSelection = Game (initialState 0)
    handle' (KeyPress "1") LevelSelection = Game (initialState 1)
    handle' (KeyPress "2") LevelSelection = Game (initialState 2)
    handle' (KeyPress "3") LevelSelection = Game (initialState 3)
    handle' (KeyPress "4") LevelSelection = Game (initialState 4)
    handle' (KeyPress "5") LevelSelection = Game (initialState 5)
    handle' (KeyPress "6") LevelSelection = Game (initialState 6)
    handle' e LevelSelection = LevelSelection
    handle' (KeyPress "L") (Game s) = LevelSelection
    handle' (KeyPress " ") (Game s) 
      | isWon s   = LevelSelection
      | otherwise = Game s
    handle' e (Game s) = Game (handle e s)
    
    draw' LevelSelection = levelSelectScreen
    draw' (Game s) = draw s
    
helpMain :: Integer -> Picture
helpMain p = translated 0 8 (scaled 2 2 (lettering "Help")) & 
    translated 0 (-7) (scaled 0.8 0.8 (lettering (pack (concat ["Page ", (show p), "/3"])))) &
    translated 0 (-8) (scaled 0.8 0.8 (lettering "Press H to close Help screen")) & 
    translated 9 0 (scaled 0.8 0.8 (rotated (pi / 2) (colored (translucent grey) (lettering "The game by kototok903"))))

helpScreen :: Integer -> Picture
helpScreen 1 = helpPage
    helpPage1 :: Picture
    helpPage1 = translated 0 4 (lettering "It is Help screen") & 
        translated 0 2 (lettering "If you open it while playing,") & 
        translated 0 0.5 (lettering "it won't reset your moves") & 
        translated 0 (-1.5) (lettering "Use Right and Left arrow keys") &
        translated 0 (-3) (lettering "to turn pages") 
helpScreen 2 = helpPage
    helpPage2 :: Picture
    helpPage2 = translated 0 2.5 (lettering "On Level Selection screen:") & 
        translated 0 0.5 (lettering "Press on the level number key (0 - 6)") & 
        translated 0 (-1) (lettering "to start the level with that number")
helpScreen 3 = helpPage
    helpPage3 :: Picture
    helpPage3 = translated 0 5 (scaled 0.8 0.8 (lettering "The goal of Sokoban is to push Boxes")) & 
        translated 0 4 (scaled 0.8 0.8 (lettering "using Player and get each of them to Storages")) & 
        translated 0 2 (lettering "While playing:") & 
        translated 0 0 (lettering "Use Arrow keys or WASD to move Player") & 
        translated 0 (-1.5) (lettering "Press Escape or R to reset current level") & 
        translated 0 (-3) (lettering "Press L to open Level Selection screen") & 
        translated 0 (-4.5) (lettering "Press Z or U to undo your last move")
helpScreen 0 = helpPage
    helpPage0 :: Picture
    helpPage0 = translated 0 1 (lettering "Fun fact:") &
        translated 0 (-0.5) (lettering "Level 6 is shaped like a fly")
helpScreen (-1) = helpPagem
    helpPagem1 :: Picture
    helpPagem1 = translated 0 0.5 (lettering "Have you heard about Konami Code?") & 
        translated 0 (-1) (scaled 0.8 0.8 (lettering "(Enter — Start)"))
helpScreen p = blank
     
data HelpState world = HelpOpened Integer world | HelpClosed Integer world
  deriving Eq

withHelp :: Activity (LevelState world) -> Activity (HelpState (LevelState world))
withHelp (Activity is handle draw) = (Activity is' handle' draw'
    is' = HelpClosed 1 is
    
    handle' (KeyPress "H") (HelpClosed p s) = HelpOpened p s
    handle' (KeyPress "H") (HelpOpened p s) = HelpClosed p s
    handle' (KeyPress "Right") (HelpOpened 3 s) = HelpOpened 3 s
    handle' (KeyPress "Right") (HelpOpened p s) = HelpOpened (p + 1) s
    handle' (KeyPress "Left") (HelpOpened (-1) s) = HelpOpened (-1) s
    handle' (KeyPress "Left") (HelpOpened p s) = HelpOpened (p - 1) s
    handle' e (HelpOpened p s) = HelpOpened p s
    handle' e (HelpClosed p s) = HelpClosed p (handle e s)
    
    draw' (HelpOpened p _) = helpMain p & helpScreen p
    draw' (HelpClosed p s) = draw s

data UndoState world = UndoState world (List world)
  deriving Eq

withUndo :: Eq world => Activity world -> Activity (UndoState world)
withUndo (Activity is handle draw) = (Activity is' handle' draw'
    is' = UndoState is Empty
    
    handle' (KeyPress "U") s = undo s
    handle' (KeyPress "Z") s = undo s
    handle' e olds@(UndoState s ss) = let news = handle e s in 
      if news /= s
      then UndoState (handle e s) (Entry s ss)
      else olds
    
    undo s@(UndoState _ Empty) = s
    undo (UndoState _ (Entry s ss)) = UndoState s ss
    
    draw' (UndoState s _) = draw s

data KCState world = KCProgress Integer world
  deriving Eq

withKC :: Activity (UndoState (HelpState (LevelState State))) -> Activity (KCState (UndoState (HelpState (LevelState State))))
withKC (Activity is handle draw) = (Activity is' handle' draw'
    is' = KCProgress 0 is
    
    handle' e@(KeyPress "Up") (KCProgress 0 s) = KCProgress 1 (handle e s)
    handle' e@(KeyPress "Up") (KCProgress 1 s) = KCProgress 2 (handle e s)
    handle' e@(KeyPress "Down") (KCProgress 2 s) = KCProgress 3 (handle e s)
    handle' e@(KeyPress "Down") (KCProgress 3 s) = KCProgress 4 (handle e s)
    handle' e@(KeyPress "Left") (KCProgress 4 s) = KCProgress 5 (handle e s)
    handle' e@(KeyPress "Right") (KCProgress 5 s) = KCProgress 6 (handle e s)
    handle' e@(KeyPress "Left") (KCProgress 6 s) = KCProgress 7 (handle e s)
    handle' e@(KeyPress "Right") (KCProgress 7 s) = KCProgress 8 (handle e s)
    handle' (KeyPress "B") (KCProgress 8 s) = KCProgress 9 s
    handle' (KeyPress "A") (KCProgress 9 s) = KCProgress 10 s
    handle' (KeyPress "Enter") (KCProgress 10 s) = KCProgress 11 s
    handle' e (KCProgress 11 (UndoState (HelpClosed p _) ss)) = KCProgress 0 (UndoState (HelpClosed p (Game (initialState (-1)))) ss)
    handle' e (KCProgress 11 (UndoState (HelpOpened p _) ss)) = KCProgress 0 (UndoState (HelpClosed p (Game (initialState (-1)))) ss)
    handle' e (KCProgress n s) = KCProgress n (handle e s)
    
    draw' (KCProgress n s) = draw s 

sokoban :: Activity State
sokoban = Activity (initialState 0) handle draw

movePlayer :: Direction -> State -> State
movePlayer d (State l (Coord x y) _ boxes) 
  | find boxes (moveCoord d (Coord x y)) && 
    (find boxes (moveCoord d (moveCoord d (Coord x y))) || not (isEmpty (maze l (moveCoord d (moveCoord d (Coord x y))))))
    = State l (Coord x y) d boxes
  | find boxes (moveCoord d (Coord x y)) && 
    isEmpty (maze l (moveCoord d (moveCoord d (Coord x y))))
    = State l (moveCoord d (Coord x y)) d 
    (change (moveCoord d (Coord x y)) (moveCoord d (moveCoord d (Coord x y))) boxes)
  | isEmpty (maze l (moveCoord d (Coord x y))) = State l (moveCoord d (Coord x y)) d boxes
  | otherwise = State l (Coord x y) d boxes
      isEmpty :: Tile -> Bool
      isEmpty Ground = True
      isEmpty Storage = True
      isEmpty t = False

handle :: Event -> State -> State
handle (KeyPress "Up") s = movePlayer U s
handle (KeyPress "Down") s = movePlayer D s
handle (KeyPress "Left") s = movePlayer L s
handle (KeyPress "Right") s = movePlayer R s
handle (KeyPress "W") s = movePlayer U s
handle (KeyPress "A") s = movePlayer L s
handle (KeyPress "S") s = movePlayer D s
handle (KeyPress "D") s = movePlayer R s
handle _ s = s

draw :: State -> Picture
draw (State l (Coord x y) d boxes) = atCoord (Coord x y) (player d) & 
    drawBoxes l boxes & 
    drawMaze l (Coord left left) &
    translated 0 8 (lettering (pack (concat ["Level ", (if l == -1 then "???" else (show l))]))) 

runActivity :: Activity world -> IO ()
runActivity (Activity initialWorld handle draw) = activityOf initialWorld handle draw

main :: IO ()
main = runActivity (withStartScreen (withKC (withUndo (withHelp (withLevels (resetable (withWin sokoban)))))))