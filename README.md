## Haskell parser generator based on the ALL(*) parsing algorithm.

### sample grammar

S -> Ac | Ad

A -> aA | b

#### Input/output examples


```haskell
*Test.AllStarTests> parse ['a', 'b', 'c'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']], Leaf 'c'])
```

```haskell
*Test.AllStarTests> parse ['b', 'd'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'b'], Leaf 'd'])
```


```haskell
*Test.AllStarTests> parse ['a', 'a', 'a', 'a', 'b', 'c'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']]]]], Leaf 'c'])
```