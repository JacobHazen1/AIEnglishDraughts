module Main where

import Checkers
import GameLogic


main :: IO ()

main = blackAi black_ai apply_move initialGameState

-- main = aiTest red_ai black_ai apply_move initialGameState

-- main = human apply_move g4



states ::GameState -> [GameState]
states st =  map ((flip apply_move) st) (moves st)



minmax :: GameState -> Integer -> Int
minmax st 0 = red_heuristic st
minmax st d = case _status st of 
        Red -> maximum (map (\a -> minmax a (d-1) ) (states st))
        Black -> minimum (map (\a -> minmax a (d-1) ) (states st))


bot = -300000
top = 300000

{--------------------------------------------------------------------------------------------------------------------}

red_heuristic :: GameState -> Int   
red_heuristic st =  length (_redPieces st) -  length( _blackPieces st) + 2*(length(_redKings st) - length(_blackKings st))            
  
black_heuristic :: GameState -> Int
black_heuristic st = length (_blackPieces st) - length( _redPieces st) + 2*(length(_blackKings st) - length(_redKings st))


black_ai::GameState -> Move
black_ai st = snd (foldl movingmin (top, []) (moves st))
 where 
        movingmin(startmin, start) next = case (nextmin st next > startmin) of
                True -> (startmin, start)
                False -> (nextmin st next, next)  

nextmin :: GameState -> Move -> Int
nextmin st next = (minmax (apply_move next st) 3)

nextmax :: GameState -> Move -> Int
nextmax st next = (minmax (apply_move next st) 3)

                
red_ai :: GameState -> Move
red_ai st = snd (foldl movingmax (bot, []) (moves st))
 where
        movingmax (startmax, start) next = 
                case (nextmax st next > startmax) of
                True -> (nextmax st next, next)
                False -> (startmax, start)  




{---------------------------------------------------------------------------------------------------------------------}

apply_move :: Move -> GameState -> GameState 
apply_move m s = case _status s of
  Red | moves s == [] -> s{_status = GameOver, _message = "Black Wins"} 
      | (m`elem` (moves s)) -> if moves s == simple_moves s 
        then setMessage $ (make_simple_move m s) 
        else setMessage $ (make_jump_move m s)
      | otherwise -> s{_message = "Error Invalid Move"}
  Black | moves s == [] -> s{_status = GameOver, _message = "Red Wins"} 
        | (m`elem` (moves s)) -> if moves s == simple_moves s 
          then setMessage $ (make_simple_move m s) 
          else setMessage $ (make_jump_move m s)
        | otherwise -> s{_message = "Error Invalid Move"}
{---------------------------------------------------------------------------------------------------------------}

moves::GameState -> [Move]
moves game 
 | jumpmoves /= [] = jumpmoves
 | otherwise = simplemoves where
  simplemoves = simple_moves game
  jumpmoves = jump_moves game

simple_moves::GameState -> [Move]
simple_moves game 
 | _status game == Red = 
  ([[(a,b),(a+1,b-1)] | (a,b) <-_redPieces game, checkonboard(a+1,b-1) game, checkifEmpty (a+1,b-1) game] ++
  [[(a,b),(a-1,b-1)] | (a,b)<-_redPieces game, checkonboard(a-1,b-1) game, checkifEmpty(a-1,b-1) game] ++ 
  simple_movesKings game)
 | otherwise = 
  ([[(a,b), (a+1,b+1)] | (a,b) <-_blackPieces game, checkonboard(a+1,b+1) game, checkifEmpty(a+1,b+1) game]) ++ 
  ([[(a,b),(a-1,b+1)] | (a,b)<-_blackPieces game, checkonboard(a-1,b+1) game, checkifEmpty(a-1,b+1)game] ++ 
  simple_movesKings game) 

checkifEmpty::(Int,Int) -> GameState -> Bool
checkifEmpty (x,y) game = case _status game of
  Red |(x,y) `elem` _redPieces game -> False
      |(x,y) `elem` _blackPieces game->False
      |(x,y) `elem` _redKings game -> False
      |(x,y) `elem` _blackKings game ->False 
      |otherwise -> True
  Black | (x,y) `elem` _redPieces game -> False 
        | (x,y) `elem` _blackPieces game-> False
        | (x,y) `elem` _redKings game -> False
        | (x,y) `elem` _blackKings game ->False 
        | otherwise -> True
checkonboard :: (Int, Int) -> GameState -> Bool
checkonboard (x,y) game 
 |  x >= 0 && 7 >= x && y >= 0 && 7 >= y = True
 | otherwise = False

checkOpposition :: (Int, Int) -> GameState -> Bool
checkOpposition (x,y) game = case _status game of
  Red | (x,y) `elem` _blackPieces game -> True
      | (x,y) `elem` _blackKings game -> True
      | otherwise -> False
  Black | (x,y) `elem` _redPieces game -> True
        | (x,y) `elem`_redKings game -> True
        | otherwise -> False


simple_movesKings:: GameState-> [Move]
simple_movesKings game
 |_status game == Red = 
   ([[(a,b),(a+1,b-1)] | (a,b) <-_redKings game, checkonboard(a+1,b-1) game, checkifEmpty (a+1,b-1) game]) ++
   ([[(a,b),(a-1,b-1)] | (a,b)<-_redKings game, checkonboard(a-1,b-1) game, checkifEmpty(a-1,b-1)game]) ++ 
   ([[(a,b), (a+1,b+1)] | (a,b) <-_redKings game, checkonboard(a+1,b+1) game, checkifEmpty(a+1,b+1) game]) ++ 
   ([[(a,b),(a-1,b+1)] | (a,b)<-_redKings game, checkonboard(a-1,b+1) game, checkifEmpty(a-1,b+1)game]) 
 |otherwise = 
   ([[(a,b),(a+1,b-1)] | (a,b) <-_blackKings game, checkonboard(a+1,b-1) game, checkifEmpty (a+1,b-1) game]) ++
   ([[(a,b),(a-1,b-1)] | (a,b)<-_blackKings game, checkonboard(a-1,b-1) game, checkifEmpty(a-1,b-1)game]) ++ 
   ([[(a,b), (a+1,b+1)] | (a,b) <-_blackKings game, checkonboard(a+1,b+1) game, checkifEmpty(a+1,b+1) game]) ++ 
   ([[(a,b),(a-1,b+1)] | (a,b)<-_blackKings game, checkonboard(a-1,b+1) game, checkifEmpty(a-1,b+1)game]) 

jumpPawns:: GameState-> Coord -> [Coord] -> Coord -> [Move]
jumpPawns st start rem (x,y) = case _status st of 
         Red ->
             [(x'',y''):ys
             | ((x',y'),(x'',y'')) <- [((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
             , not (elem (x',y') rem) && checkOpposition (x',y') st && (start==(x'',y'') || checkifEmpty (x'',y'') st && checkonboard (x'',y'') st)
             , ys <- if checkRedKing(x'',y'') then jump_over (jumpKing st start ((x',y'):rem) (x'',y'')) else jump_over (jumpPawns st start ((x',y'):rem) (x'',y'')) ]
         Black ->
             [(x'',y''):ys
             | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]
             , not (elem (x',y') rem) && checkOpposition (x',y') st && (start==(x'',y'') || checkifEmpty (x'',y'') st && checkonboard (x'',y'') st)
             , ys <-if checkBlackKing(x'',y'') then jump_over (jumpKing st start ((x',y'):rem) (x'',y'')) else jump_over (jumpPawns st start ((x',y'):rem) (x'',y'')) ]
     
jumpKing:: GameState-> Coord ->[Coord]->Coord->[Move]
jumpKing st start rem (x,y) =
                   [(x'',y''):ys
                   | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                   , not (elem (x',y') rem) && checkOpposition (x',y') st && (start==(x'',y'') || checkifEmpty (x'',y'') st && checkonboard (x'',y'') st)
                   , ys <- jump_over (jumpKing st start ((x',y'):rem) (x'',y'')) ]

              
jumpKing' st xs = [(x,y):ys | (x,y) <- xs, ys <- jumpKing st (x,y) [] (x,y)]   
jumpPawns' st xs = [(x,y):ys | (x,y) <- xs, ys <- jumpPawns st (x,y) [] (x,y)]


jump_moves :: GameState -> [Move]
jump_moves st
                | _status st == Red
                    = (jumpKing' st (_redKings st))++(jumpPawns' st (_redPieces st))
                | _status st == Black
                    = (jumpKing' st (_blackKings st))++ (jumpPawns' st (_blackPieces st))
                | otherwise = [] 

make_jump_move:: Move -> GameState -> GameState
make_jump_move (start:(next:rest)) st

--RedKings
       | _status st == Red &&  start `elem` (_redKings st)
                = make_jump_move (next:rest)
                            (st{_blackKings = byebye (jumped start next) (_blackKings st)
                               ,_blackPieces = byebye (jumped start next) (_blackPieces st)
                               ,_redKings = next:(byebye start (_redKings st))
                               ,_message = ""})
--blackKings
        | _status st == Black &&  start `elem` (_blackKings st)
                = make_jump_move (next:rest)
                            (st{_redKings = byebye (jumped start next) (_redKings st)
                               ,_redPieces = byebye (jumped start next) (_redPieces st)
                               ,_blackKings = next:(byebye start (_blackKings st))
                               ,_message = ""})
--RedPieces
        | _status st == Red &&  start `elem` (_redPieces st) && checkRedKing next == False
                = make_jump_move (next:rest)
                            (st{_blackKings = byebye (jumped start next) (_blackKings st)
                               ,_blackPieces = byebye (jumped start next) (_blackPieces st)
                               ,_redPieces = next:(byebye start (_redPieces st))
                               ,_message = ""})
--BlackPieces                               
        | _status st == Black &&  start `elem` (_blackPieces st) && checkBlackKing next == False
                = make_jump_move (next:rest)
                            (st{_redKings = byebye (jumped start next) (_redKings st)
                               ,_redPieces = byebye (jumped start next) (_redPieces st)
                               ,_blackPieces = next:(byebye start (_blackPieces st))
                               ,_message = ""})                             
--RedPiecetoKing
        | _status st == Red &&  start `elem` (_redPieces st) && checkRedKing next
                 = make_jump_move(next:rest)
                            (st{_blackKings = byebye (jumped start next) (_blackKings st)
                               ,_blackPieces = byebye (jumped start next) (_blackPieces st)
                               ,_redPieces = (byebye start (_redPieces st))
                               ,_redKings = next:(_redKings st)
                               ,_message = ""})
--BlackPiecetoKing
        | _status st == Black &&  start `elem` (_blackPieces st) && checkBlackKing next 
                 = make_jump_move (next:rest)
                            (st{_redKings = byebye (jumped start next) (_redKings st)
                               ,_redPieces = byebye (jumped start next) (_redPieces st)
                               ,_blackPieces = (byebye start (_blackPieces st))
                               ,_blackKings = next:(_blackKings st)
                               ,_message = ""})           

make_jump_move _ st = st{_status = change_player st}
                    

jumped :: (Int,Int) -> (Int, Int) -> (Int, Int)                            
jumped (a,b) (c,d)
 | (a+2,b-2) == (c, d) = (a+1,b-1)
 | (a-2,b-2) == (c, d) = (a-1,b-1)
 | (a+2,b+2) == (c, d) = (a+1,b+1)
 | (a-2,b+2) == (c, d) = (a-1,b+1)

jump_over [] = [[]]
jump_over z = z
 
 
byebye _ [] = []
byebye x(y:ys) 
  | x==y = byebye x ys
  | otherwise = y : byebye x ys
 


checkRedKing (a,b) = if (a,b) == (a,0) then True else False

checkBlackKing (a,b) = if (a,b) == (a,7) then True else False



make_simple_move :: Move -> GameState -> GameState
make_simple_move [start,end] st
       | _status st == Red && start `elem` (_redKings st)
               = st{_redKings = end : byebye start (_redKings st)
                     ,_status = change_player st
                     ,_message = ""}
       | _status st == Black && start `elem` (_blackKings st)
               = st{_blackKings = end : byebye start (_blackKings st)
                     ,_status = change_player st
                     ,_message = ""}  
       | _status st == Red && start `elem` (_redPieces st)  && checkRedKing end == False
               = st{_redPieces = end : byebye start (_redPieces st)
                     ,_status = change_player st
                     ,_message = ""}                                
       | _status st == Black && start `elem` (_blackPieces st)  && checkBlackKing end == False
               = st{_blackPieces = end : byebye start (_blackPieces st)
                     ,_status = change_player st
                     ,_message = ""}  
       | _status st == Red && start `elem` (_redPieces st) && checkRedKing end
               = st{_redPieces = byebye start (_redPieces st)
                     , _redKings = end:(_redKings st)
                     ,_status = change_player st
                     ,_message = ""}       
       | _status st == Black && start `elem` (_blackPieces st) && checkBlackKing end
               = st{_blackPieces = byebye start (_blackPieces st)
                     ,_blackKings = end : (_blackKings st)
                     ,_status = change_player st
                     ,_message = ""}  
                    


change_player st = if _status st == Red then Black else Red














