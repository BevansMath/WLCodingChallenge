# Wolfram Coding Challenges
## Here is a synopsis of the Wolfram Coding Challenges and the proposed solutions 

## Challenge 1: Odds and Evens
###### Create a function that takes an unsorted list of integers and sorts them by odds first, evens second. The function should fail non-integers.

## Solution:
```
OddsAndEvens[list_]:= If[AllTrue[list, IntegerQ[#]&],SortBy[list,If[EvenQ[#],{1,2}]&], If[Length[list<3],HoldAll]];
OddsAndEvens[list]
```
###### Synopsis: Function takes a list of unsorted integers, uses the pure function #& to parse over the list, failing if the function contains non integers and if the list is less than 3

## Challenge 2: Sinusoidal functions
###### Description: Have a function output the values of a sinusoidal function in a key-value pair

##Solution:
```
SetAttributes[rules, HoldAll]
rules = Association [{Amp ->"Amplitude", Pd -> "Period", Ph -> "Phase", Dis -> "Displacement"}];
Amp [a_Integer]:= Abs[a]
Pd[b_Integer]:= 2Pi/b;
Ph[c_Integer]:= -C/B;
Dis[d_Integer]:= MatchQ[d, _Integer_];
f[x_] := Module[{y},y=FullForm[Amp*Sin[Pd*x+ph]+Dis];
TrigProperties[f[x]]:=While[Pd >0,If[Pd>0,If[Pd == 0,Break[]]; Print["Failed"];
If[Amp == 0,Break[]]; Print["Failed"]];
f[x]/@rules;
TrigProperties[]
```
###### Synopsis: The key-value pare describes the rules of the sinusoidal function, where they keys are global variables. The function f[x_] outputs the sinusoidal function as a function of y, where TrigProperties[f[x]] defines the limitations of the sinusoidal function.

## Challenge 3: Rolling Dice
###### Create a function that outputs a random dice roll of n dice, along with the probability of each nth roll

## Solution:

```
Quit[]

Dice[n_Integer]:= Dice[n,Black]
Format[Dice[n_Integer,c]]:= With[{dots = {1->{5}, 2 -> {3, 7}, 3 -> {3, 5, 7}, 4 -> {1, 3, 7, 9}, 
    5 -> {1, 3, 5, 7, 9}, 6 -> {1, 2, 3, 7, 8, 9}} /. 
   l : {__Integer} :> 
    Sequence @@ Thread[l -> Graphics[{c, Disk[]}, ImageSize -> 16]], 
  face = Partition[Range@9, 3]}, 
 Panel[Grid[face /. {n /. dots, _Integer -> Null}, ItemSize -> All]]]
Dice /@ Range@6 // Row

ClearAll[RollDice]

RollDice[n_Integer]:= {(1/6)^n,
Multicolumn[Dice/@RandomVariate[DiscreeUniformDistribution[{1,6}],n]]}

RollDice[n_]:= Table[Dice[],{n}]

RollDice[9]

RollDice[216]

RollDice[3]

RollDice[2]
```
###### Synopsis: The first function, Dice[n_Integer] allows us to create a graphic visualization of a regular die with black dots in the appropriate array positions
###### The RollDice[n_Integer] function utilizes the fact that the probability of n dice rolls is equal to (1/6) to the nth power for n amount of die. We use RandomVariate[DiscreteUniformDistribution[{1/6}],n] to compute for n many dice

## Challenge 4: A Knights Shortest Path
###### Create an explanation of a mathematical problem. I chose a Knights shortest path for this challenge

##Solution: 
###### The goal is to find the minimum number of moves needed for a chess knight to go from one corner of a 100 x 100 board.
###### Next, create the variable for the knight traversing the chessboard. Note that each move decreases the "Manhatten Distance" between the starting point and the destination.
###### We want to visit all the vertexes at least once to get the minimum distance needed. We utilize the built-in function FindHamiltonianPath to do so, and highlight the path from one diagonal to the other.
###### The function created should output the highlighted traversal path and the minimum steps taken to get from the starting point to the destination.

###### Code:

```
KnightsTour[i_IntegerQ, j_IntegerQ]:= Module[{m},m=i+j;
	If[j>m, FullForm[2*((j-m)/3)+m]; Fullform[m-2*((j-m/4)],
		If[i==1 & j==1,3;
		If[i==2 & j==2,4;]]]];
travPath = FindHamiltonianPath[KnightsTour[1,1]];
Manipulate[Show[{Chessboard,HighlightGraph[KnightsTour,PathGraph[KnightsTour[[;;i]]],
PlotTheme -> "Marketing", GraphHighlightStyle->"DehiglightHide",
EdgeStyle -> Thickness[0.1]]}],{i,100,Length[travPath],100},SaveDefinitions->True]
```
