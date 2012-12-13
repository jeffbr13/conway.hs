-- conway.hs
-- Conway's Game of Life, implemented in Haskell!
-- -----------------------------------------------------------------------------
module Main where

import Data.List



main :: IO ()
main = playGame glider



-- Game logic & stuff:
-- -----------------------------------------------------------------------------

playGame :: Board -> IO ()
playGame bd = mapM_ printBoard (game bd)


-- `game` is a (potentially infinite) list of boards
game :: Board -> [Board]
game bd = iterate iterateBoard bd


iterateBoard :: Board -> Board
iterateBoard []     = error ("The game of life has ended... " ++
                        "It looks like the only winning move is not to play!")
iterateBoard bd = onlyActiveCells [iterateCell c (length (livingNeighboursOf affCells c)) | c <- affCells]
                    where
                    affCells = affectedCells bd




iterateCell :: Cell -> Int -> Cell
iterateCell (ActiveCell p q) n
    | n < 2         = EmptyCell {x=p, y=q}
    | n <= 3        = ActiveCell {x=p, y=q}
    | n > 3         = EmptyCell {x=p, y=q}
iterateCell (EmptyCell p q) n
    | n == 3    = ActiveCell {x=p, y=q}
    | otherwise = EmptyCell {x=p, y=q}







-- Data structures:
-- -----------------------------------------------------------------------------



data Cell   = ActiveCell { x :: Int, y :: Int}
            | EmptyCell { x :: Int, y:: Int}
            --deriving Show -- (replaced by ¡MONSTER GRAPHICS ENGINE!)
            deriving Eq


type Board  = [Cell]






-- Board and row operations:
-- -----------------------------------------------------------------------------

onlyActiveCells :: Board -> Board
onlyActiveCells bd = [c | c <- bd, isActive c]


affectedCells :: Board -> [Cell]
affectedCells bd    = nubBy smPosition $ concat [(bd `cellsAround` c) | c <- bd]

cellsAround :: [Cell] -> Cell -> [Cell]
bd `cellsAround` c    = unionBy smPosition bd (mkEmptyNeighbours c)

-- --------------------------------
--prop_affectedCells :: Board -> Bool
--prop_affectedCells bd = bd == (onlyActiveCells $ affectedCells bd)



square :: Board -> Board
square bd   = unionBy smPosition bd [EmptyCell {x=p, y=q} | p <-[(minimum ps)..(maximum ps)], q <-[(minimum qs)..(maximum qs)]]
            where
            (ps,qs) = unzip [(x c, y c) | c <- bd]





-- Cell helper functions:
-- -----------------------------------------------------------------------------

class Position a where
    isIn    :: a -> [a] -> Bool
    nbrs    :: a -> a -> Bool
    smPosition   :: a -> a -> Bool


instance Position Cell
    where
    isIn  cell []     = False
    isIn  cell (c:cs)
        | cell `smPosition` c   = True
        | otherwise     = cell `isIn` cs
    -- ---------------------------------
    c1 `nbrs` c2  = nbrCellCriteria (x c1) (y c1) (x c2) (y c2)
    -- ------------------------------------------------------
    c1 `smPosition` c2    = smPositionCellCriteria (x c1) (y c1) (x c2) (y c2)
    -- -----------------------------------------------------------


-- Helpers for Cell's instantiation of `Position`
-- ------------------------------------------------
nbrCellCriteria :: Int -> Int -> Int -> Int -> Bool
nbrCellCriteria p q r s = and [ (p - r) <= 1
                                , (p - r) >= (-1)
                                , (q - s) <= 1
                                , (q - s) >= (-1)
                                , (p /= r) || (q /= s) ]
smPositionCellCriteria :: Int -> Int -> Int -> Int -> Bool
smPositionCellCriteria p q r s = and [(p == r), (q==s)]
-- ------------------------------------------------




new :: Cell -> Cell
new c   = ActiveCell {x=(x c), y=(y c)}

kill :: Cell -> Cell
kill c  = EmptyCell {x=(x c), y=(y c)}

isActive :: Cell -> Bool
isActive (ActiveCell _ _)   = True
isActive (EmptyCell _ _)    = False




-- Neighbouring cells ------------------------
livingNeighboursOf :: [Cell] -> Cell -> [Cell]
bd `livingNeighboursOf` cell     = [c | c <- bd, cell `nbrs` c, isActive c]

allNeighboursOf :: [Cell] -> Cell -> [Cell]
bd `allNeighboursOf` c = delete c (bd `cellsAround` c)


-- -------------------------------

mkEmptyNeighbours :: Cell -> [Cell]
mkEmptyNeighbours c = [EmptyCell {x=r, y=s} | r <- [p-1..p+1], s <- [q-1..q+1], (r /= s) || (s /= 0)]
                    where
                    p = x c
                    q = y c
----------------------------------------------








-- ¡MONSTER GRAPHICS ENGINE! below:
-- -----------------------------------------------------------------------------



instance Show Cell where
    show (ActiveCell _ _)   = " X"
    show (EmptyCell _ _)    = " ·"
    --show (ActiveCell x y)   = " (X "++(show x)++","++(show y)++")"
    --show (EmptyCell x y)    = " (· "++(show x)++","++(show y)++")"

printBoard :: Board -> IO ()
printBoard bd   = do
    putStr "\n"
    putStr (replicate 10 ' ')
    let board = rows $ square $ affectedCells bd
    let xValues = [ x c | c <- (board !! 0)]
    mapM_ (putStr . (" " ++) . show) xValues
    putStr "\n\n"
    mapM_ (printRow) board
    putStr "\n"


printRow :: [Cell] -> IO ()
printRow row     = do
    let yValue = show (y (row !! 0))
    putStr (yValue ++ replicate (10 - (length yValue)) ' ' )
    mapM_ (putStr . show) row
    putStr "\n"




-- Sorting the board into rows -------
rows :: Board -> [[Cell]]
rows    = groupBy sameRow . sortBoardY


sameRow :: Cell -> Cell -> Bool
sameRow c1 c2   =   y c1 == y c2
-- -----------------------------------


-- Board & cell sorting
-- -------------------------
sortBoardY :: Board -> Board
sortBoardY = sortBy sortCellY


sortCellX :: Cell -> Cell -> Ordering
sortCellX c1 c2
    | x c1 < x c2       = LT
    | x c1 > x c2       = GT
    -- 'X's must be equal
    | y c1 < y c2       = LT
    | y c1 > y c2       = GT
    -- 'Y's must also be equal
    | otherwise         = EQ

sortCellY :: Cell -> Cell -> Ordering
sortCellY c1 c2
    | y c1 < y c2       = LT
    | y c1 > y c2       = GT
    -- 'Y's must be equal
    | x c1 < x c2       = LT
    | x c1 > x c2       = GT
    -- 'X's must also be equal
    | otherwise         = EQ
-- -------------------------








-- Sample items
-- -----------------------------------------------------------------------------

newCell :: Int -> Int -> Cell
newCell x y = ActiveCell {x=x, y=y}


-- blinker, period 2
blinker = [newCell (-1) 0, newCell 0 0, newCell 1 0]

-- glider
glider = [newCell 0 0, newCell 1 1, newCell 2 1, newCell 0 2, newCell 1 2]

-- Light-Weight Space Ship
lwss = [newCell 0 0, newCell 3 0,
        newCell 4 1, newCell 4 2, newCell 4 3,
        newCell 0 2, newCell 1 3, newCell 2 3, newCell 3 3]
