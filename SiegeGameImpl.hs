{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module SiegeGameImpl where
import Game
import SiegeGame
import SiegeGameScore1

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (tails, partition)

data ScoreAlg = Score0 | Score1 deriving (Show,Read)
getGame :: ScoreAlg -> SiegeGame
getGame Score0 = defaultGame
getGame Score1 = case defaultGame of SiegeGame g _ -> SiegeGame g (score1 g)

---

-- Task 1.1)
placeDefenders :: [V] -> Map V Player -> Map V Player
placeDefenders [] s = s
placeDefenders (v:vs) s = placeDefenders vs (M.insert v False s)

-- Task 1.2)
simpleMove :: V -> V -> Map V Player -> Map V Player
simpleMove v0 v1 s
    | M.lookup v0 s == Nothing = s
	| otherwise = M.insert v1 p (M.delete v0 s) where Just p = M.lookup v0 s
	

-- Task 1.3)
captureMove :: V -> [V] -> Map V Player -> Map V Player
captureMove v [] s = s
captureMove v (x:xs) s 				 
    | M.lookup v s == Just False = captureMove v (x:xs) (M.delete v s)
    | otherwise = captureMove c xs (M.insert c False (M.delete x s)) where c = captureHelper v x

captureHelper :: (V) -> (V) -> (V)
captureHelper (x,y) (a,b) = (2*a - x, 2*b - y)

-- Task 1.4)
moveImpl :: SiegeMove -> Map V Player -> Map V Player
moveImpl (PlaceDefenders vs) s = placeDefenders vs s
moveImpl (SimpleMove v0 v1) s = simpleMove v0 v1 s
moveImpl (CaptureMove v0 v1) s = captureMove v0 v1 s

-- Task 1.5)
startStateImpl :: SiegeGame -> Map V Player
startStateImpl sg = M.fromList (zip vs ps) 
    where 
        vs = inhabitedVertices(getGameVertices sg)
        ps = replicate (length (getGameVertices defaultGame)) True

inhabitedVertices :: [V] -> [V]
inhabitedVertices [] = []
inhabitedVertices (x:xs)
    | inG x = inhabitedVertices xs
	| otherwise = (x) : inhabitedVertices xs

getGameVertices :: SiegeGame -> [V]
getGameVertices (SiegeGame g _) = [ v | (v, _) <- M.toList g ]

-- Task 1.6)
placeDefenderMoves :: SiegeGame -> [SiegeMove]
placeDefenderMoves sg = placeFirstDefender (startStateImpl sg) (getGameVertices sg) []

placeFirstDefender :: Map V Player -> [V] -> [SiegeMove] -> [SiegeMove]
placeFirstDefender s [] ml = ml
placeFirstDefender s (v:vs) ml
    | M.lookup v s /= Just True = placeFirstDefender s vs (placeSecondDefender v vs s ml)
    | otherwise = placeFirstDefender s vs ml
	
placeSecondDefender :: V -> [V] -> Map V Player -> [SiegeMove] -> [SiegeMove]
placeSecondDefender v [] s ml = ml
placeSecondDefender v (x:xs) s ml
    | M.lookup x s /= Just True && x /= v = placeSecondDefender v xs s (ml ++ [PlaceDefenders[v,x]])
    | otherwise = placeSecondDefender v xs s ml

-- Task 1.7)
simpleMoves :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
simpleMoves sg@(SiegeGame g _) p s = findPlayerPiece (getGameVertices sg) p g s []

findPlayerPiece :: [V] -> Player -> Map V VertexInfo -> Map V Player -> [SiegeMove] -> [SiegeMove]
findPlayerPiece [] p g s sm = sm
findPlayerPiece (v:vs) p g s sm
    | M.lookup v s == Just p = findPlayerPiece vs p g s (findEmptyNeighbor v p (neighbors (fromJust (M.lookup v g))) s sm)
    | otherwise = findPlayerPiece vs p g s sm

findEmptyNeighbor :: V -> Player -> [NeighborInfo] -> Map V Player -> [SiegeMove] -> [SiegeMove]	
findEmptyNeighbor v p [] s sm = sm
findEmptyNeighbor v p (i:is) s sm
    | p == True && a == True && M.lookup n s == Nothing = findEmptyNeighbor v p is s (sm ++ [SimpleMove v n])
	| p /= True && M.lookup n s == Nothing = findEmptyNeighbor v p is s (sm ++ [SimpleMove v n])
	| otherwise = findEmptyNeighbor v p is s sm
	 where
        n = getNeighbor i
        a = isAttackerAllowed i

-- Task 1.8)
defenderCaptureMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderCaptureMoves sg@(SiegeGame g _) s = defenderCaptureMovesFindDefender (getGameVertices sg) g s []

defenderCaptureMovesFindDefender :: [V] -> Map V VertexInfo -> Map V Player -> [SiegeMove] -> [SiegeMove]
defenderCaptureMovesFindDefender [] g s sm = sm
defenderCaptureMovesFindDefender (v:vs) g s sm
    | M.lookup v s == Just False = defenderCaptureMovesFindDefender vs g s sm ++ (defenderCaptureMovesFindAttackerNeighbor v (lookupNeighborInfoList v g) g s [])
    | otherwise = defenderCaptureMovesFindDefender vs g s sm

defenderCaptureMovesFindAttackerNeighbor ::  V -> [NeighborInfo] -> Map V VertexInfo -> Map V Player -> [SiegeMove] -> [SiegeMove]
defenderCaptureMovesFindAttackerNeighbor v [] g s sm = sm
defenderCaptureMovesFindAttackerNeighbor v@(x,y) (i:is) g s sm
    | M.lookup n s == Just True && M.lookup e s == Nothing && elem e (map getNeighbor(lookupNeighborInfoList n g)) = defenderCaptureMovesFindAttackerNeighbor v is g s (sm ++ [CaptureMove v ([n]++(defenderCaptureMovesFindNextAttacker e (lookupNeighborInfoList n g) g (M.delete v (M.delete n s)) []))])
	| otherwise = defenderCaptureMovesFindAttackerNeighbor v is g s sm
	 where
        n@(a,b) = getNeighbor i
        e = captureHelper v n

defenderCaptureMovesFindNextAttacker ::  V -> [NeighborInfo] -> Map V VertexInfo -> Map V Player -> [V] -> [V]
defenderCaptureMovesFindNextAttacker v [] g s sm = sm
defenderCaptureMovesFindNextAttacker v@(x,y) (i:is) g s sm
    | M.lookup n s == Just True && M.lookup e s == Nothing && elem e (map getNeighbor(lookupNeighborInfoList n g)) = sm ++ [n] ++ (defenderCaptureMovesFindNextAttacker e (lookupNeighborInfoList e g) g (M.delete v (M.delete n s)) [])
	| otherwise = defenderCaptureMovesFindNextAttacker (x,y) is g s sm
	 where
        n@(a,b) = getNeighbor i
        e = captureHelper v n
				
lookupNeighborInfoList :: V -> Map V VertexInfo -> [NeighborInfo]
lookupNeighborInfoList v g = neighbors (fromJust (M.lookup v g))

-- Task 1.9)
showGameImpl :: SiegeGame -> Map V Player -> String
showGameImpl (SiegeGame g _) m = boardString [(x, y) | y <- reverse [-3..3], x <- [-3..3]] ""
    where
        boardString :: [V] -> String -> String
        boardString [] s = s
        boardString (v:vs) s
            | not (inV v) = " " ++ formatHelper (fst v) vs s
            | isGoal (fromJust (M.lookup v g)) = goalString v ++ formatHelper (fst v) vs s
            | otherwise = restString v ++ formatHelper (fst v) vs s
        formatHelper i l s
            | i == 3 = ("\n" ++ boardString l s)
            | otherwise  = boardString l s
        goalString v
            | M.lookup v m == Nothing = "*"
            | M.lookup v m == Just True = "A"
            | otherwise = "D"
        restString v
            | M.lookup v m == Nothing = "."
            | M.lookup v m == Just True = "a"
            | otherwise = "d"		

---

defenderMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderMoves sg s = case [v | (v, False) <- M.toList s] of
  [] -> placeDefenderMoves sg
  _  -> case defenderCaptureMoves sg s of
    [] -> simpleMoves sg False s
    xs -> xs

movesImpl :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
movesImpl sg True s    = simpleMoves sg True s
movesImpl sg False s   = defenderMoves sg s

valueImpl :: SiegeGame -> Player -> Map V Player -> Double
valueImpl sg p m | null $ movesImpl sg p m = if p then -infinity else infinity
valueImpl (SiegeGame g _) _ m | and [ M.lookup v m == Just True | (v,vi) <- M.toList g, isGoal vi ] = infinity
valueImpl (SiegeGame g s) p m = s m


instance Game SiegeGame where
  type GameState SiegeGame  = Map V Player
  type Move SiegeGame       = SiegeMove

  startState    = startStateImpl
  showGame      = showGameImpl
  move _ _ s m  = moveImpl m s
  moves         = movesImpl
  value         = valueImpl

	