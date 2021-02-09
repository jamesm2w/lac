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
-- To simply evaluate the expressions I just pattern match for the two constructors for Action type:
-- if its the Add constructor, then I add the value to the computer, if it matches the Sub constructor 
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
-- My result implementation calls a local function which has an integer parameter to accumulate the value. 
-- This accumulate function just returns the accumulator when called with an empty list, but if the list has >= 1
-- elements it pattern matches the head and tail of the list and returns the apply function on the head of the list with the accumulator
-- and adds it to the recursive call of the accumulate function on the tail.
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
-- My cells implementation just returns a list of the two possible cell states - Enabled or Disabled
-- I use pattern matching on the MkCell constructor to get the action parameter. Since the original state of the
-- cell is irrelevant I use the underscore to match any enabled state. In the list I use the explicit constructor
-- with enabled as True for one element and False for the other.
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
{-
  My candidates implementation first pattern matches on the list parameter, if its an empty list, then it returns a 2D list containing the empty list.
  If the list isn't empty then it calls a local function with a list of all possible cells' states. The permute function pattern matches on
  a non-empty 2D-list, for the head element and the tail. The head represents a list of two elements returned from `states` , so I pattern match
  again on both elements, the first being the enabled state and the second being the disabled state. 

  The function then returns the recursive call of the permuted states with the enabled state cons'd, appended to the recursive call on the tail of
  the list with the disabled state cons'd. Noting how these two branches needed the same parameter in the form of the recursive call to permute, I added
  a local bound definition for `permutedStates` which is the recursive call. Finally if permute is called on an empty list, or some other case,
  I used a wildcard pattern to return a list containing the empty list - this acts as the base case of the recursion.
-}
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates cells = permute [states x | x <- cells]
  where
    permute :: [[Cell]] -> [[Cell]]
    permute ([enabled, disabled] : remainingStates) = map (enabled :) permutedStates ++ map (disabled :) permutedStates
      where
        permutedStates :: [[Cell]]
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
-- My implementation of `solveRow` creates a list of rows using the MkRow constructor, the original target 
-- and a the list of cells that comes from the `candidates` function with the predicate guard that the
-- the accumulated result returned by applying the `result` function must be equal to the target.
-- This returns a list of rows which are all possible solutions.
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
  My implementation of solve first pattern matches on the MkGrid constructor, if either of the targets or cells are the empty list then
  it just returns a list of that grid.

  If the grid is not empty, then it returns a list of grids, with the same column totals, and a list of rows from all the possible solutions of rows,
  with the predicate guard that the columns of the rows are equal to the their column totals (since the possible solutions of rows are guaranteed to 
  match the row targets it is not necessary to check that again).

  For the list of all possible row solutions I define a list through a comprehension which applies `solveRow` to each row in the row parameter of the Grid.
  This gives me a 2D list where each element is the possibke solutions for each row. To transform this into a list of solutions for the whole grid
  I then use another `permute` function. This function takes the lists of rows and pattern matches the empty list which is the base case, returning a 2D list
  containing the empty list.

  The `permute` function then pattern matches the head (first) and tail (rest): The head is a list of unknown length with each element being a solution 
  to the row. While the tail is the same for the subsequent rows. To get all possible solutions for the grid, I want to add each element (solution) of 
  the head row to all the other solutions of the grid. To achieve this, I use a list comprehension, with two generators. The first generator is the 
  "other solutions" to the grid, returned by the recursive call on the tail. The second is the solutions in the head element. Then these two elements
  are joined using the cons operator, resulting in a list of all solutions to the grid.

  Now I have a list of possible solutions to the grid, however, the problem is that these solutions are not guaranteed to fit the column constraints.
  I tried several different approaches to this problem, like enumerating all possible column solutions and then checking which solutions were identical
  However, I thought this was too inefficient and had me using operations like indexing. Instead, I went for the more simple option which involves checking
  if the columns in the row solution match the column total and excluding them if they don't.

  To achieve this, I created the `checkColumns` function, which takes a column set and the totals and checks they match. The first base case of this function
  is that both lists are empty, in this case return True. It then pattern matches for the head of both list. It then checks that the `result` of the column
  is equal to the target. It returns that with the logical `and` of the recursive call on the tails of the lists. Finally it has a wilcard case, which would
  be matched if the lists were different lengths, in this case it returns false, since lists of different length can't match.

  The final piece of the puzzle is how to get the column lists. To do this I created a helper function which got a 2D list of cells from the grid's list of rows.
  Then I used the Data.List function `transpose` to transpose the rows/columns. This created the right structure for the checkColumns function. Which completes
  the list comprehension.

-}
---------------------------------------
solve :: Grid -> [Grid]
solve g@(MkGrid [] _) = [g]
solve g@(MkGrid _ []) = [g]
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

-- | `toCells` @row list@ returns the list of rows with each element being a list of cells in that row rather than of @row@ type
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> toCells ([row0, row1])
-- [ [MkCell False (Add 3), MkCell False (Add 5)], [MkCell False (Add 2), MkCell False (Add 2)] ]
---------------------------------------
-- This helper function `toCells` essentially removes the row construct around cells in the list to more easily allow
-- the cells to be used for functions like transpose - or to allow Grids to be directly treated like a 2-D grid of cells.
-- It does this by pattern matching for the case where there are no rows left, where it returns the empty list.
-- Then it matches a head which is again pattern matched for the cells list inside, and the tail. It returns the cells list
-- cons with the recursive call of the function applied to the tail.
---------------------------------------
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
-- My implementation of `rotate` first pattern matches on the case that the list is empty, here it can just return the empty list.
-- If the list is not empty then it pattern matches on the direction of rotation. If the direction is "L" for left, then it matches
-- the head and tail of the list. Returning the tail appended to the singleton list of the head. Essentially moving the first element 
-- the end of the list. Thus rotating it left by 1.
-- If the direction is "R" for right, then it uses the `last` function to get the last element of the list, and cons it to the result of
-- `init` called with the list (this returns all but the last element of the list). Thus moving the last element to the front, rotating 
-- the list right by 1.
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
-- ONLY HAVE TO ROTATE L by ONE
{-
  My `rotations` implementation first pattern matches on the grid, if either part of the grid is the empty list then it just returns the singleton
  list of that grid.
  Then for any other grid, it returns the rotatedGrid definition. Which is defined as the list of grids with the rows rotated appended to the list
  of grids with the columns rotated.

  For the grids with the rows rotated, it uses a list comprehension to create a grids with the original column totals and row lists from the `rotations'`
  function. The rotations' function takes the list of rows and returns a list of list of rows, where each element has a different row rotated.
  It does this through recursion: the base case is that the list only has one element (row), in which case it just returns the the list of that row rotated
  left by one -- to simplify the definitions I made the `rotateRow` function as well which calls `rotate` on the passed row.
  If there is more than one element, it returns the rotated row cons the tail, consd with the map of the row consd to the recusive call. This ensures that
  all of the later recursive calls have the current row unrotated in the correct place.
  Finally I define a wildcard case which just returns the list containing the empty list if the other cases aren't matched.

  For the grids with the columns rotated, I perform the same operation but instead of calling rotations' on the rows from the grid, I define `cols` to
  be the grouped cells of the transposed cell list with the column totals. To do this I defined the function `grpCells` which takes a 2D list of cells
  and the list of the totals and returns a list of the row type with the respective rows cells and total put together. To get the 2D cell list, I used
  the previously defined `toCells` function. Finally I used the Data.List `transpose` function to swap the rows and columns.

  However, there was one issue after calling rotations' on the cols, the fact the list was arranged by columns and not by rows, so I had to perform 
  the same transformation again: extracting cells from Row type, calling transpose, grouping the cells into rows again with the row totals. Incidentally,
  I also defined the `rTots` to store the total of each row, through a list comprehension extracting the total from the row list parameter of the Grid type.

  Finally, to simplify the final statement and remove some parentheses, I defined a new function `transform` which applies the three functions on the passed
  row list and target list.  
-}
---------------------------------------
rotations :: Grid -> [Grid]
rotations g@(MkGrid [] _) = [g]
rotations g@(MkGrid _ []) = [g]
rotations (MkGrid cTots rows) = rotatedGrids
  where 

    rotatedGrids :: [Grid]
    rotatedGrids = [ MkGrid cTots rotation | rotation <- rotations' rows ] ++
      [ MkGrid cTots (transform rotation rTots) | rotation <- rotations' (transform rows cTots)]

    rTots :: [Int]
    rTots = [t | (MkRow t _) <- rows]

    grpCells :: [[Cell]] -> [Int] -> [Row]
    grpCells (c:cs) (t:ts) = MkRow t c : grpCells cs ts
    grpCells _ _ = []

    rotations' :: [Row] -> [[Row]]
    rotations' [r] = [[rotateRow r]] -- only need this one possibility. if the normal "r" was included, that leaves an unrotated grid.
    rotations' (r:rs) = (rotateRow r : rs) : map (r :) (rotations' rs)
    rotations' _ = [[]]

    rotateRow :: Row -> Row
    rotateRow (MkRow t cs) = MkRow t (rotate L cs)

    transform :: [Row] -> [Int] -> [Row]
    transform rs ts = grpCells(transpose (toCells rs)) ts

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
{-
  For this function I wanted to approach it like a Breadth-First-Search. However, the issue was I needed the trace
  of rotations to get to the target grid, which would be trivial in a Depth-First-Search, however in BFS the nodes 
  are not necessarily traversed in the right order to just output that. My solution was to use a tuple. Each element
  in the queue had it's first element as the sequence of grids which by 1 rotation returned the second element, the 
  actual grid itself.

  So to begin, the main `steps` implementation calls the internal `bfs` function, with the intial grid's rotations.
  To transform this list of grids into the queue, I used map, and `pair`. Pair takes two arguments and returns a pair.
  In this case the first argument was always the empty list since this was the first rotation, and the second argument was
  supplied from the `rotations` call on grid. Finally, in the seen grids it just passes the singleton with the current grid.

  Then in the bfs function it pattern matches for if the queue is empty. In this case it returns the empty list. If the queue 
  has one element, it pattern matches that one element for the first element which is the trace, and the second which is the current grid.
  This is the base case for the recursion and returns the trace with the singleton of the current grid. 

  The final case is the case where the list has more than 1 element. It pattern matches on the head, and the two elements in the pair being
  the trace and the grid, as well as the tail. The first check is that the grid is a solution, if it is, then it returns the trace with the 
  head of the solution appended. This is because solution is defined as the result of `solve` applied to the grid, so could contain multiple
  solutions/results, of which only one needs to be returned. The check for the solution is defined as `isSolution` this checks that the solution
  is not equal to the empty list. If it were the empty list this is indicative of solve not being able to find a solution.

  If the current grid is not a solution then it calls the function recursively with the new `queue` and `seen` values.
  The new `queue` is defined as the current queue, with the `filteredNextNodes` mapped into pairs with the new `trace`
  The new `trace` is simply the current trace with the singleton list of the current grid appended. The `filteredNextNodes`
  is defined as the list of the rotations of all the current grid, filtered by those which have already been seen by the program.

  To filter the nodes already seen, I defined the new function `filterGrids` which takes the list of grids, the list of seen grids
  and returns the filtered list which only contains elements not elements of the seen list. To check for this it uses `notElem` which 
  is defined as the inverse of the `elem` function. I decided to use it infix to make it easier to read.

  Finally the new `seen` is defined as the old seen list appended with the new filtered nodes. 
-}
---------------------------------------
steps :: Grid -> [Grid]
steps grid = bfs (map (pair []) (rotations grid)) [grid]
  where

    --             Grid Queue ->  Seen  -> Output Trace
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
    filterGrids seen gs = filter (`notElem` seen) gs

    pair :: [Grid] -> Grid -> ([Grid], Grid)
    pair gs g = (gs, g)
--------------------------------------------------------------------------------