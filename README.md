A 24 puzzle solver that lists all solutions without duplication.

## Algorithm

Represent expressions as binary trees. Find all solutions using depth-first search.

Expressions like a+b and b+a that can be converted into each other by the commutative property or the associative property should be considered the same. Here we introduce a data structure that uniquely represents the same expressions:

- a±b => 0{+a, ±b}
- (a±<sub>1</sub>b)±<sub>2</sub>c => 0{+a, ±<sub>1</sub>b, ±<sub>2</sub>c}
- c±<sub>2</sub>(a±<sub>1</sub>b) => 0{±<sub>2</sub>a, ±<sub>1</sub>±<sub>2</sub>b, +c}
- a×b => 1{×a, ×b}
- (a+b)÷c => 1{×(a+b), ÷c} => 1{×0{+a, +b}, ÷c}
- ...

and so on.

Expressions should have fewest parentheses. When expressions like a-(b+c) occur, the subtree can be skipped because there are always the same expressions with fewer parentheses like a-b-c.
