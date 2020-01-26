module Moves where

    --import Lens.Micro.Platform -- DON'T!!!
    
import GameLogic


-- Generates all moves based on game state
moves :: GameState -> [Move]
moves st
      | jump_moves st /= [] = jump_moves st
      | otherwise = simple_moves st


-- Generates all jump moves
jump_moves :: GameState -> [Move]
jump_moves st
             | _status st == Red
              = (jumpKing (_redKings st) st) ++ (jumpSimple (_redPieces st) st) 
             | _status st == Black
              = (jumpKing (_blackKings st) st) ++ (jumpSimple (_blackPieces st) st) 
             | otherwise = [] 

-- Generates all simple moves
simple_moves:: GameState -> [Move]
simple_moves st
               |_status st == Red
                = (simpleKing_moves (_redKings st) st)++(simplePieces_move (_redPieces st) st)
               |_status st == Black
                = (simpleKing_moves (_blackKings st) st)++(simplePieces_move (_blackPieces st) st)
               | otherwise = []


-- delete a piece from the board
delete :: Coord -> [Coord] -> [Coord]
delete y [] = []
delete y (x:xs) = if x == y then xs else x : delete y xs

-- gets coordinate of jumped piece
jumped :: Coord -> Coord -> Coord
jumped first second = (((fst(first) + fst(second)) `quot` 2),((snd(first) + snd(second)) `quot` 2))

-- change status
changePlayer :: Status-> Status
changePlayer Red = Black
changePlayer Black = Red

-- replaces piece with new piece
replace :: Coord -> Coord -> [Coord] -> [Coord]
replace start end (a:as) | a == start = end:as 
                         | otherwise = a:replace start end as

-- Checks if a piece is a member 
member :: (Ord a) => a -> [a] -> Bool
member x [] = False
member x (a:as) | x == a = True
                | otherwise = member x as

-- Generates all jump moves for king
jumpKing:: [Coord] -> GameState -> [Move]
jumpKing xs st = [(x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y) st]
jumpKing' start j (x,y) st =
          [ (x'',y''):ys
          | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
          , not (member (x',y') j) && opponent_occupied (x',y') st &&  ((emptyPosition (x'',y'') st) || start == (x'',y'')) && onBoard (x'',y'')
          , ys <- jump_over (jumpKing' start ((x',y'):j) (x'',y'') st) ]



-- Generates all jump moves for pawns
jumpSimple:: [Coord] -> GameState -> [Move]
jumpSimple xs st = [(x,y):ys | (x,y) <- xs, ys <- jumpSimple' (x,y) [] (x,y) st]
jumpSimple' start j (x,y) st =
          [ (x'',y''):ys
          | ((x',y'),(x'',y'')) <- [((x+1,y+(dir st)),(x+2,y+(dir' st))),((x-1,y+(dir st)),(x-2,y + (dir' st)))]
          , ys <- 
          if (opponent_occupied (x',y') st && atEnd(x'',y'') st && ((emptyPosition (x'',y'') st) || start == (x'',y'')) && onBoard (x'',y'')) 
          then jump_over (jumpKing' start ((x',y'):j) (x'',y'') st) 
          else if  (opponent_occupied (x',y') st && ((emptyPosition (x'',y'') st) || start == (x'',y'')) && onBoard (x'',y'')) 
          then jump_over (jumpSimple' start ((x',y'):j)  (x'',y'') st) 
          else   []]

jump_over [] = [[]]
jump_over z = z



-- Checks if spot is occupied by opponent
opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied x st 
          | _status st == Red && member x (_blackPieces st) = True
          | _status st == Red && member x (_blackKings st) = True
          | _status st == Black && member x (_redPieces st) = True
          | _status st == Black && member x (_redKings st) = True
          | otherwise = False

-- Checks if piece is at end of board
atEnd :: Coord -> GameState -> Bool
atEnd (x,y) st | _status st == Red && y == 0 = True
               | _status st == Black && y == 7 = True
               | otherwise = False


-- Generates simple moves for pawns
simplePieces_move :: [Coord] -> GameState -> [Move]
simplePieces_move c st = [[(x,y),(x',y')] | (x,y) <- c, (x',y') <- [(x+1,y+ (dir st)),(x-1,y+ (dir st))], emptyPosition (x',y') st, onBoard(x',y')]

-- Generates all simple moves for kings
simpleKing_moves :: [Coord] -> GameState -> [Move]
simpleKing_moves c st = [[(x,y),(x',y')] | (x,y) <- c, (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)], emptyPosition (x',y') st, onBoard(x',y')]


-- Checks empty position
emptyPosition :: Coord -> GameState -> Bool
emptyPosition pos st = not (member pos (_redPieces st)) && not (member pos (_blackPieces st)) && not (member pos (_redKings st)) && not (member pos (_blackKings st))


-- Checks if on board
onBoard :: Coord -> Bool
onBoard (x,y) | x >= 0 && x < 8 && y >= 0 && y < 8 = True
              | otherwise = False

-- Direction based on status
dir :: GameState -> Int
dir st = case (_status st) of
               Red -> -1
               Black -> 1

dir' :: GameState -> Int
dir' st = case (_status st) of
                Red -> -2
                Black -> 2              


    