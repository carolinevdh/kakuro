Kakuro
======

"Kakuro or Kakkuro (Japanese: カックロ) is a kind of logic puzzle that is often referred to as a mathematical transliteration of the crossword." - Wikipedia :)

This Haskell library takes empty Kakuro puzzles and solves them, either in concurrent or non-concurrent fashion.


Usage
-----

To run, the built version of kakuro.lhs expects one or two command line arguments: ./kakuro <filename> <width>. <width> is optional and should contain the width of the puzzle in case it is not square. The file referenced at <filename> is expected to be of the following format: [( Int , Int , Int ), ...] where every (value,horizontal,vertical) tuple represents a cell. value is -2 if a cell is not part of the puzzle, -1 if a cell is part of the puzzles problem (then horizontal contains the expected sum for the following row and/or vertical contains the expected sum for the column below), 0 if is yet to be filled in or finally, 1 to 9 if it has been filled in. For example, a simple 4 by 4 kakuro puzzle is internally represented as [(-2,0,0),(-2,0,0),(-1,0,7),(-1,0,6),(-2,0,0),(-1,4,4),(0,0,0),(0,0,0),(-1,7,0),(0,0,0),(0,0,0),(0,0,0),(-1,6,0),(0,0,0),(0,0,0),(0,0,0)] and does not contain any newlines.



Optimizations
-------------

* Because Haskell is very fast at retrieving the first item of a list and at building lists one element at a time, the solver is using two lists of (Int,Int,Int) at all times. In the beginning, the first list is empty and the second list is our input puzzle. The first cell to be reviewed is in front of the second list. When we are done processing this cell, it is put in front of the first list and removed from the second list. As a result, the next cell to be processed is in front of the second list. In the end, our solved puzzle can be found in the first list, with all cells in reversed order.

* When calculating the possible values for a cell, after a list of unused values is retrieved, the solver calculates sets of possible values from these unused values that can still be filled in to achieve the row/column sum. This is implemented in okPermute by making permutations of these values for a length that is equal to the amount of empty cells in the current row/sum. This calculation greatly improves the algorithm speed over a more naive backtracking implementation that considers the possible values to be all the unused ones.



About the concurrent solver in kakuropar.lhs
--------------------------------------------

The concurrent version of kakuro solver can be found in kakuropar.lhs and differs by just a few characters. Where the non-concurrent version uses map in solve:
<pre><code>map (\val -> solve ((val,0,0):puzzleDone) xs puzzleLength puzzleSide) okVals</code></pre>
the concurrent version uses:
<pre><code>parMap rdeepseq (\val -> solve ((val,0,0):puzzleDone) xs puzzleLength ...</code></pre>,
effectively running different branches of the algorithm concurrently.