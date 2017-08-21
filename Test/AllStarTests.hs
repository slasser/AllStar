-- To do: put in the right expected results for these tests!

module Test.AllStarTests where

import Test.HUnit
import ParserGenerator.AllStar

--------------------------------TESTING-----------------------------------------

atnS = [[(INIT 'S', EPS, CHOICE 'S' 1),
         (CHOICE 'S' 1, NT 'A', MIDDLE 1),
         (MIDDLE 1, T 'c', MIDDLE 2),
         (MIDDLE 2, EPS, FINAL 'S')],
          
        [(INIT 'S', EPS, CHOICE 'S' 2),
         (CHOICE 'S' 2, NT 'A', MIDDLE 3),
         (MIDDLE 3, T 'd', MIDDLE 4),
         (MIDDLE 4, EPS, FINAL 'S')]]

atnA = [[(INIT 'A', EPS, CHOICE 'A' 1),
         (CHOICE 'A' 1, T 'a', MIDDLE 5),
         (MIDDLE 5, NT 'A', MIDDLE 6),
         (MIDDLE 6, EPS, FINAL 'A')],
          
        [(INIT 'A', EPS, CHOICE 'A' 2),
         (CHOICE 'A' 2, T 'b', MIDDLE 7),
         (MIDDLE 7, EPS, FINAL 'A')]]

atnEnv = [(NT 'S', atnS), (NT 'A', atnA)]


-- For now, I'm only checking whether the input was accepted--not checking the derivation.

-- Example from the manual trace of ALL(*)'s execution
parseTest1 = TestCase (assertEqual "for parse [a, b, c],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'a',
                                               Node 'A'
                                                [Leaf 'b']],
                                             Leaf 'c']))
                                   (parse ['a', 'b', 'c'] (NT 'S') atnEnv True))
                                   
-- Example #1 from the ALL(*) paper
parseTest2 = TestCase (assertEqual "for parse [b, c],"
                                    (Right (Node 'S'
                                             [Node 'A'
                                               [Leaf 'b'],
                                              Leaf 'c']))
                                    (parse ['b', 'c'] (NT 'S') atnEnv True))
                                    
-- Example #2 from the ALL(*) paper
parseTest3 = TestCase (assertEqual "for parse [b, d],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'b'],
                                             Leaf 'd']))
                                   (parse ['b', 'd'] (NT 'S') atnEnv True))
                                    
-- Input that requires more recursive traversals of the A ATN
parseTest4 = TestCase (assertEqual "for parse [a a a b c],"
                                   (Right (Node 'S'
                                            [Node 'A'
                                              [Leaf 'a',
                                               Node 'A'
                                                [Leaf 'a',
                                                 Node 'A'
                                                  [Leaf 'a',
                                                   Node 'A'
                                                    [Leaf 'b']]]],
                                             Leaf 'c']))
                                   (parse ['a', 'a', 'a', 'b', 'c'] (NT 'S') atnEnv True))

-- Make sure that the result of parsing an out-of-language string has a Left tag.             
parseTest5 = TestCase (assertEqual "for parse [a b a c],"
                                   True
                                   (let parseResult = parse ['a', 'b', 'a', 'c'] (NT 'S') atnEnv True
                                        isLeft pr = case pr of
                                                      Left _ -> True
                                                      _ -> False
                                    in  isLeft parseResult))

conflictsTest = TestCase (assertEqual "for getConflictSetsPerLoc()"
                         
                                      ([[(MIDDLE 5, 1, []), (MIDDLE 5, 2, []),(MIDDLE 5, 3, [])],
                                        [(MIDDLE 5, 1, [MIDDLE 1]), (MIDDLE 5, 2, [MIDDLE 1])],
                                        [(MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])]])
                                         
                                      (getConflictSetsPerLoc (D [(MIDDLE 5, 1, []),
                                                                 (MIDDLE 5, 2, []),
                                                                 (MIDDLE 5, 3, []),
                                                                 (MIDDLE 5, 1, [MIDDLE 1]),
                                                                 (MIDDLE 5, 2, [MIDDLE 1]),
                                                                 (MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])])))

prodsTest = TestCase (assertEqual "for getProdSetsPerState()"
                     
                                  ([[(MIDDLE 5, 1, []),
                                     (MIDDLE 5, 2, []),
                                     (MIDDLE 5, 3, []),
                                     (MIDDLE 5, 1, [MIDDLE 1]),
                                     (MIDDLE 5, 2, [MIDDLE 1])],
                                     
                                    [(MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])]])
                                         
                                  (getProdSetsPerState (D [(MIDDLE 5, 1, []),
                                                           (MIDDLE 5, 2, []),
                                                           (MIDDLE 5, 3, []),
                                                           (MIDDLE 5, 1, [MIDDLE 1]),
                                                           (MIDDLE 5, 2, [MIDDLE 1]),
                                                           (MIDDLE 7, 2, [MIDDLE 6, MIDDLE 1])])))


ambigATN = [[(INIT 'S', EPS, CHOICE 'S' 1),
             (CHOICE 'S' 1, T 'a', MIDDLE 1),
             (MIDDLE 1, EPS, FINAL 'S')],

            [(INIT 'S', EPS, CHOICE 'S' 2),
             (CHOICE 'S' 2, T 'a', MIDDLE 2),
             (MIDDLE 2, EPS, FINAL 'S')],

            [(INIT 'S', EPS, CHOICE 'S' 3),
             (CHOICE 'S' 3, T 'a', MIDDLE 3),
             (MIDDLE 3, T 'b', MIDDLE 4),
             (MIDDLE 4, EPS, FINAL 'S')]]

ambigEnv = [(NT 'S', ambigATN)]

ambigParseTest1 = TestCase (assertEqual "for parse [a],"
                                        True
                                        (let parseResult = parse ['a'] (NT 'S') atnEnv True
                                             isLeft pr = case pr of
                                                           Left _ -> True
                                                           _ -> False
                                         in  isLeft parseResult))

ambigParseTest2 = TestCase (assertEqual "for parse [a b],"
                                        (Right (Node 'S'
                                                 [Leaf 'a',
                                                  Leaf 'b']))
                                        (parse ['a', 'b'] (NT 'S') ambigEnv True))

        
tests = [TestLabel "parseTest1"    parseTest1,
         TestLabel "parseTest2"    parseTest2,
         TestLabel "parseTest3"    parseTest3,
         TestLabel "parseTest4"    parseTest4,
         TestLabel "parseTest5"    parseTest5,
                  
         TestLabel "conflictsTest" conflictsTest,
         TestLabel "prodsTest"     prodsTest,

         TestLabel "ambigParseTest1" ambigParseTest1,
         TestLabel "ambigParseTest2" ambigParseTest2]
       
main = runTestTT (TestList tests)
