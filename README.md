## Haskell parser generator based on the ALL(*) parsing algorithm.

### sample grammar

S -> Ac | Ad

A -> aA | b

#### Input/output examples

i : `['a', 'b', 'c']`

o : `Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']], Leaf 'c']`



i : `['b', 'c']`

o : `Node 'S' [Node 'A' [Leaf 'b'], Leaf 'c']`



i : `['a', 'a', 'a', 'b', 'c']`

o : `Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']]]], Leaf 'c']`