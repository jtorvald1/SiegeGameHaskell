{-# LANGUAGE FlexibleContexts                  #-}
module GameStrategies where
import Game
import Data.List (intercalate)

import SiegeGameImpl
import SiegeGame

data Tree m v   = Tree v [(m,Tree m v)]
type GTree g    = Tree (Move g) (Player,GameState g)

type AlphaBeta = (Value,Value)
type Depth = Int

instance (Show m, Show v) => Show (Tree m v) where
  show (Tree x xs) = "Tree " ++ show x ++ " ["++ sub' ++"]"
    where
      sub = intercalate ",\n" (map show xs)
      sub' = if null sub
                then ""
                else "\n" ++ unlines (map ("  " ++ ) $ lines sub)


startTree :: Game g => g -> Player -> GTree g
startTree g p = tree g (p, startState g)

-- Task 2.1)
tree :: Game g => g -> (Player, GameState g) -> GTree g
tree g ps@(p,s) = Tree ps (zip (moves g p s) (map (tree g) st)) where st = movedStatePlayerZip g p (generateMovedGameStateList g p s (moves g p s) [])

movedStatePlayerZip :: Game g => g -> Player -> [GameState g] -> [(Player, GameState g)]
movedStatePlayerZip g p gs = zip ps gs
    where
        ps = replicate (length gs) (not p)

generateMovedGameStateList :: Game g => g -> Player -> GameState g -> [Move g] -> [GameState g] -> [GameState g]
generateMovedGameStateList g p s [] gs = gs
generateMovedGameStateList g p s (x:xs) gs = generateMovedGameStateList g p s xs (gs ++ [(move g p s x)])

-- Task 2.2)
takeTree :: Depth -> Tree m v -> Tree m v
takeTree 0 (Tree v _) = Tree v []
takeTree _ (Tree v []) = Tree v []
takeTree n (Tree v ch) =
    Tree v (map (foo (n - 1)) ch)
  where
    foo n (m, t) = (m, takeTree n t)

-- Task 2.3)
minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax = undefined

--minimax g t =

-- Task 2.4)
minimaxAlphaBeta :: Game g => g -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBeta = undefined
