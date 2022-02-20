# Wolfram Coding Challenges
### Here is a synopsis of the Wolfram Coding Challenges and the proposed solutions 

#Challenge 1: Odds and Evens
## Create a function that takes an unsorted list of integers and sorts them by odds first, evens second. The function should fail non-integers.

## Solution:
### `OddsAndEvens[list_]:= If[AllTrue[list, IntegerQ[#]&],SortBy[list,If[EvenQ[#],{1,2}]&], If[Length[list<3],HoldAll]];
OddsAndEvens[list]`
### Function takes a list of unsorted integers, uses the pure function #& to parse over the list, failing if the function contains non integers and if the list is less than 3

#Challenge 2: Sinusoidal functions
## Have a function output the values of a sinusoidal function in a key-value pair

##Solution:
### `SetAttributes[rules, HoldAll]
rules = Association [{Amp ->"Amplitude", Pd -> "Period", Ph -> "Phase", Dis -> "Displacement"}];
Amp [a_Integer]:= Abs[a]
Pd[b_Integer]:= 2Pi/b;
Ph[c_Integer]:= -C/B;
Dis[d_Integer]:= MatchQ[d, _Integer_];

f[x_] := Module[{y},y=FullForm[Amp*Sin[Pd*x+ph]+Dis];
TrigProperties[f[x]]:=While[Pd >0,If[Pd>0,If[Pd == 0,Break[]]; Print["Failed"];
If[Amp == 0,Break[]]; Print["Failed"]];
f[x]/@rules;
TrigProperties[]`

#Challenge 3: Rolling Dice
### Create a function that outputs a random dice roll of n dice, along with the probability of each nth roll

#Challenge 4: A Knights Shortest Path
### Create an explanation of a mathematical problem. I chose a Knights shortest path for this challenge

##Solution: 
### The goal is to find the minimum number of moves needed for a chess knight to go from one corner of a 100 x 100 board.
### Next, create the variable for the knight traversing the chessboard. Note that each move decreases the "Manhatten Distance" between the starting point and the destination.
### We want to visit all the vertexes at least once to get the minimum distance needed. We utilize the built-in function FindHamiltonianPath to do so, and highlight the path from one diagonal to the other.
### The function created should output the highlighted traversal path and the minimum steps taken to get from the starting point to the destination.

