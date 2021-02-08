--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where

import Data.List

--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can
-- have on a row or column total.
data Action
  = Add Int
  | Sub Int
  deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
  deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
  deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
  deriving (Eq, Show)

-- | Enumerates directions in which lists can be rotated.
data Direction = L | R
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@.
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2

---------------------------------------
-- To simply evaluate the sum expressions I just pattern match on the two constructors for Action type:
-- if its the Add constructor, then I add the accumulator with the value, if it matches the Sub constructor 
-- then the value is subtracted. 
---------------------------------------
eval :: Action -> Int -> Int
eval (Add x) acc = acc + x
eval (Sub y) acc = acc - y

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3

---------------------------------------
-- For applying the cell to the accumulator I pattern match on the MkCell constructor. If the enabled bool is 
-- True then I reuse the `eval` function to apply the action to the accumulator. If enabled is False then the 
-- accumulator can just be returned.
---------------------------------------
apply :: Cell -> Int -> Int
apply (MkCell True action) acc = eval action acc
apply (MkCell False _) acc = acc

-- | `result` @cells@ calculates the total produced by the actions of all
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4

---------------------------------------
-- My result implementation calls an internal helper function accumulate the value, starting at 0. This uses a slightly 
-- different interface including the accumulating Integer. This accumulate function uses recursion to add the
-- result of applying the head cell, to the result of the recursive call on the tail of the list.
-- In the base case it just returns accumulated value. 
---------------------------------------
result :: [Cell] -> Int
result cells = accumulate cells 0
  where
    accumulate :: [Cell] -> Int -> Int
    accumulate [] acc = acc
    accumulate (c : cs) acc = apply c acc + accumulate cs acc

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]

---------------------------------------
-- The implementation for this function is relatively simple, since each cell only has two
-- states it can be in (enabled or disabled), I just explicitly define the list of both of
-- those states.
---------------------------------------
states :: Cell -> [Cell]
states (MkCell _ action) = [MkCell True action, MkCell False action]

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
--
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]

---------------------------------------
-- I approached this by thinking about the tree-diagram which would represent choosing a state with each cell
-- Which then led me to first creating the list of all possible states of all the cells passed. Then passing them
-- to the local function "permute". This function takes the first element of the states passed, and pattern matches
-- the "enabled" and "disabled" states, it them appends the "enabled" state to all the permuted subsets of the remaining
-- sets and then joins that with the same but instead the "disabled" state is joined to all of them. I achieved that
-- by using `map` with half of the append operation. And I use recursive calls to get the full list of states of the remaining
-- cells in the list. Finally, the simple base case is that if no cells are passed, or there are no more cells left to process it
-- just returns a list with the empty list.
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates cells = permute [states x | x <- cells]
  where
    permute ([enabled, disabled] : remainingStates) = map (enabled :) permutedStates ++ map (disabled :) permutedStates
      where
        permutedStates = permute remainingStates
    permute _ = [[]]

-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)]
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]

---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
solveRow :: Row -> [Row]
solveRow (MkRow target cells) = [MkRow target soln | soln <- candidates cells, result soln == target]

-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]

---------------------------------------
{-
The first function defined for solve, is `permute` The purpose of `permute` is to create all possible "scenarios" or combination
of individual row solutions. Then I define two values which are the set of individual row solutions - `rowSolutions` and
the set of all possible Grid solutions based on the rows being correct - `possibleGridSolutionsByRow`. 

This last value is used in the list comprehension which is returned from the solve function. This list enumerates through
all the possible solutions and creates the Grid type. However, this a conditional predicate on this comprehension. 

This predicate function takes the set of rows in this solution candidate and tests if the columns sum to the column total
provided to the function. It does this by looping through the column indexes (from 0 to the length of the column totals passed)
it then makes use of a new function which returns all cells in the column (this function loops through all rows and returns a
list of all cells at the specific index), then it uses the previously defined `result` function to apply all the actions and get the accumulated result
if the result is equal to the respective column total, then a True is returned at that position in the list. Then the `all`
function applies a function to all elements in the list. If they all return True then `all` returns True. The function used
here is a simple `== True` to check if all elements are True. If they are the predicate returns True, and the solution candidate
is returned as a possible solution to the Grid.

prev solutions based on length/!! that was ineficient, seem to have shaved an average of 0.5s of run time!
-}
---------------------------------------
solve :: Grid -> [Grid]
solve (MkGrid [] a) = [MkGrid [] a]
solve (MkGrid a []) = [MkGrid a []]
solve (MkGrid colTotals rows) = [MkGrid colTotals possibleRowSet | possibleRowSet <- possibleGridSolutionsByRow, checkColumns (transpose (toCells possibleRowSet)) colTotals]
  where 
    possibleGridSolutionsByRow :: [[Row]]
    possibleGridSolutionsByRow = permute rowSolutions

    rowSolutions :: [[Row]]
    rowSolutions = [solveRow row | row <- rows] -- all the row solutions which work

    checkColumns :: [[Cell]] -> [Int] -> Bool
    checkColumns [] [] = True
    checkColumns (column:columns) (target:targets) = (target == result column) && checkColumns columns targets
    checkColumns _  _ = False

    permute :: [[Row]] -> [[Row]]
    permute [] = [[]]
    permute (first:rest) = [ solution : otherSolutions | otherSolutions <- permute rest, solution <- first ]


toCellsGrid :: Grid -> [[Cell]]
toCellsGrid (MkGrid _ rows) = toCells rows 

toCells :: [Row] -> [[Cell]]
toCells [] = [[]]
toCells ((MkRow _ cells):rows) = cells : toCells rows 

-- | `rotate` @direction list@ rotates the items in @list@ to the left or
-- right depending on the value of @direction@. For example:
--
-- >>> rotate L [1,2,3]
-- [2,3,1]
--
-- >>> rotate R [1,2,3]
-- [3,1,2]
--
-- >>> rotate L []
-- []
--
-- >>> rotate R [1]
-- [1]

---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate _ [] = []
rotate L (x:xs) = xs ++ [x]
rotate R a = last a : init a

-- | `rotations` @grid@ returns a list of grids containing all possible ways
-- to rotate @grid@. This means the resulting list should normally have
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]

---------------------------------------
-- [Add your explanation of how your implementation works here] ONLY HAVE TO ROTATE L by ONE
---------------------------------------
rotations :: Grid -> [Grid]
rotations (MkGrid [] a) = [MkGrid [] a]
rotations (MkGrid a []) = [MkGrid a []]
rotations (MkGrid cTots rows) = rotatedGrids
  where 

    rotatedGrids :: [Grid]
    rotatedGrids = [ MkGrid cTots rotation | rotation <- rotations' rows ] ++
      [ MkGrid cTots (grpCells (transpose  (toCells rotation)) rTots) | rotation <- rotations' cols]

    rTots :: [Int]
    rTots = [t | (MkRow t _) <- rows]

    cols :: [Row]
    cols = grpCells (transpose (toCells rows)) cTots

    grpCells :: [[Cell]] -> [Int] -> [Row]
    grpCells (c:cs) (t:ts) = MkRow t c : grpCells cs ts
    grpCells _ _ = []

    rotations' :: [Row] -> [[Row]]
    rotations' [r] = [[rotateRow r]] -- only need this one possibility. if the normal "r" was included, that leaves an unrotated grid.
    rotations' (r:rs) = (rotateRow r : rs) : map (r :) (rotations' rs)
    rotations' _ = [[]]

    rotateRow :: Row -> Row
    rotateRow (MkRow t cs) = MkRow t (rotate L cs)

-- | `steps` @grid@ finds the sequence of rotations that lead to a solution
-- for @grid@ in the fewest number of rotations. The resulting list includes
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)]
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)]
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ]
-- ]

---------------------------------------
-- [Add your explanation of how your implementation works here]
-- BFS - tuple fst = prev; snd = grid element
---------------------------------------
steps :: Grid -> [Grid]
steps grid = bfs (map (pair []) (rotations grid)) [grid]
  where

    --   Grid Queue -> Seen -> Output Trace
    bfs :: [( [Grid], Grid )] -> [Grid] -> [Grid]
    bfs [] _ = []
    bfs [(tr, q)] _ = tr ++ [q]
    bfs ((tr, q):qs) s = if isSolution then tr ++ [head solution] else bfs queue seen
      where 
        solution = solve q
        isSolution = solution /= []

        trace = tr ++ [q]

        filteredNextNodes = filterGrids s (rotations q)

        queue = qs ++ map (pair trace) filteredNextNodes

        seen = s ++ filteredNextNodes
    
    filterGrids :: [Grid] -> [Grid] -> [Grid]
    filterGrids _ [] = []
    filterGrids [] _ = []
    filterGrids seen gs = [ g | g <- gs, g `notElem` seen]

    pair :: [Grid] -> Grid -> ([Grid], Grid)
    pair gs g = (gs, g)
--------------------------------------------------------------------------------
