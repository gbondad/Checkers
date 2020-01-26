module CheckersJuanBondad ( moves, applyMove, red_Ai, black_Ai
                )
where

import Checkers
import Moves


black_Ai :: GameState -> Move
black_Ai st = bestMove st (moves st) [] (-30000)

red_Ai :: GameState -> Move
red_Ai st = bestMove st (moves st) [] (-30000)


black_heurestic :: GameState -> Int
black_heurestic st = (length (_blackPieces st)) - (length (_redPieces st)) + 2*((length (_blackKings st)) - (length (_redKings st)))

red_heurestic :: GameState -> Int
red_heurestic st = (length (_redPieces st)) - (length (_blackPieces st)) + 2*((length (_redKings st)) - (length (_blackKings st)))



minimax :: GameState -> Int -> Bool -> Status -> Int
minimax state depth isMaximizing player
                | player == Black && (depth == 0 || (length (moves state) == 0))  = black_heurestic state
                | player == Red && (depth == 0 || (length (moves state) == 0)) = red_heurestic state
                | isMaximizing == True = maximum(map (\x -> (minimax (applyMove x state) (depth-1) (False) player)) (moves state))
                | isMaximizing == False = minimum(map (\x -> (minimax (applyMove x state) (depth-1) (True) player)) (moves state))  


bestMove :: GameState -> [Move] -> Move -> Int -> Move
bestMove st [] m' s = m'
bestMove st (m:ms) m' s 
                | score > s = bestMove st ms m score
                | otherwise = bestMove st ms m' s
                where score = (minimax (applyMove m st) 5 False (_status st))
                

{--
minimax :: GameState -> Int -> Bool -> Status -> Int
minimax state depth isMax player
                | player == Black && (depth == 0 || (length (moves state) == 0))  = black_heurestic state
                | player == Red && (depth == 0 || (length (moves state) == 0)) = red_heurestic state
                | isMax == True = maximize state (moves state) [] (-1000) (depth - 1) False player
                | isMax == False = minimize state (moves state) [] (1000) (depth - 1) True player


-- return highest evalutation of all possible moves to current game state
maximize :: GameState -> [Move] -> Move -> Int -> Int -> Bool -> Status -> Int
maximize state [] bmove s depth isMax p = s
maximize state (m:ms) bmove s depth isMax p
                | score > s = maximize state ms m score depth isMax p
                | otherwise = maximize state ms bmove s depth isMax p
                where score = minimax (applyMove m state) (depth-1) False p



-- return smallest evalutation of all possible moves to current game state
minimize :: GameState -> [Move] -> Move -> Int -> Int -> Bool -> Status -> Int
minimize state [] bmove s depth isMax p = s
minimize state (m:ms) bmove s depth isMax p
                | score < s = minimize state ms m score depth isMax p
                | otherwise = minimize state ms bmove s depth isMax p
                where score = minimax (applyMove m state) (depth-1) True p


        

-- return move with highest evaluation
bestMove :: GameState -> [Move] -> Move -> Int -> Move
bestMove st [] m' s = m'
bestMove st (m:ms) m' s 
                | score > s = bestMove st ms m score
                | otherwise = bestMove st ms m' s
                where score = (minimax (applyMove m st) 4 False (_status st))

-}

-- Applies a move to the current game state
applyMove :: Move -> GameState -> GameState
applyMove m st | moves st == [] = st{_status = GameOver}
               | member m (moves st) && member m (simple_moves st) = make_simple_move m st
               | member m (moves st) && member m (jump_moves st) = make_jump_move m st
               | member m (moves st) = st{_message = "Legal move but its not applying"}
               | otherwise = st{_message = "Illegal move!!"}


-- Function apply simple move to gamestate
make_simple_move :: Move -> GameState -> GameState
make_simple_move [start,end] st
  | _status st == Red && member start (_redPieces st) && atEnd end st = st
        {_redPieces = delete start (_redPieces st),
        _redKings = end:(_redKings st),
        _status = changePlayer (_status st),
        _message = ""
        }
  | _status st == Black && member start (_blackPieces st) && atEnd end st = st 
        {_blackPieces = delete start (_blackPieces st),
        _blackKings = end:(_blackKings st),
        _status = changePlayer (_status st),
        _message = ""
        } 
  | _status st == Red && member start (_redPieces st) = st
        {_redPieces = replace start end (_redPieces st),
        _status = changePlayer (_status st),
        _message = ""
        }
  | _status st == Black && member start (_blackPieces st) = st
        {_blackPieces = replace start end (_blackPieces st),
        _status = changePlayer (_status st),
        _message = ""
        } 
  | _status st == Red && member start (_redKings st) = st
        {_redKings = replace start end (_redKings st),
        _status = changePlayer (_status st),
        _message = ""
        }
  | _status st == Black && member start (_blackKings st) = st
        {_blackKings = replace start end (_blackKings st),
        _status = changePlayer (_status st),
        _message = ""
        } 

-- apply jump move to game state
make_jump_move :: Move -> GameState -> GameState
make_jump_move(start:(next:rest)) st
    | start:(next:rest) == [] = st {_status = changePlayer (_status st)}
    |_status st == Black && member start (_blackPieces st) && atEnd start st -- Black Piece Transform to King
        = make_jump_move (next:rest)
            (st{_redKings = delete (jumped start next) (_redKings st),
            _redPieces = delete (jumped start next) (_redPieces st),
            _blackPieces = delete start (_blackPieces st),
            _blackKings = next:(_blackKings st),
            _message = ""})
    |_status st == Red && member start (_redPieces st) && atEnd start st -- Red Piece Transform to King
        = make_jump_move (next:rest)
            (st{_blackKings = delete (jumped start next) (_blackKings st),
            _blackPieces = delete (jumped start next) (_blackPieces st),
            _redPieces = delete start (_redPieces st),
            _redKings = next:(_redKings st),
            _message = ""})
    |_status st == Red && member start (_redPieces st) -- Red Piece Jump Move
        = make_jump_move (next:rest)
            (st{_blackKings = delete (jumped start next) (_blackKings st),
            _blackPieces = delete (jumped start next) (_blackPieces st),
            _redPieces = next:(delete start (_redPieces st)),
            _message = ""})
    |_status st == Black && member start (_blackPieces st) -- Black Piece Jump Move
        = make_jump_move (next:rest)
            (st{_redKings = delete (jumped start next) (_redKings st),
            _redPieces = delete (jumped start next) (_redPieces st),
            _blackPieces = next:(delete start (_blackPieces st)),
            _message = ""})
    | _status st == Red && member start (_redKings st) -- Red King Jump Move
        = make_jump_move (next:rest)
            (st{_blackKings = delete (jumped start next) (_blackKings st)
            ,_blackPieces = delete (jumped start next) (_blackPieces st)
            ,_redKings = next:(delete start (_redKings st))
            ,_message = ""})
    | _status st == Black && member start (_blackKings st) -- Black King Jump Move
        = make_jump_move (next:rest)
            (st{_redKings = delete (jumped start next) (_redKings st)
            ,_redPieces = delete (jumped start next) (_redPieces st)
            ,_blackKings = next:(delete start (_blackKings st))
            ,_message = ""})
make_jump_move [] st = st{_status = changePlayer(_status st)}
make_jump_move [z] st 
    | atEnd z st && member z (_blackPieces st) 
    = st{_blackPieces = delete z (_blackPieces st)
    ,_blackKings = z:(_blackKings st)
    ,_status = changePlayer(_status st)}
    | atEnd z st && member z (_redPieces st) 
    = st{_redPieces = delete z (_redPieces st)
    ,_redKings = z:(_redKings st)
    ,_status = changePlayer(_status st)}
    | otherwise = st{_status = changePlayer(_status st)}


