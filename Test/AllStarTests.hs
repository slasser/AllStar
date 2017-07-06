import Test.HUnit
import AllStar

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


-- Example from the manual trace of ALL(*)'s execution
parseTest1 = TestCase (assertEqual "for parse [a, b, c],"
                              (Just True, [])
                              (parse ['a', 'b', 'c'] (NT 'S') atnEnv))


-- Example #1 from the ALL(*) paper
parseTest2 = TestCase (assertEqual "for parse [b, c],"
                              (Just True, [])
                              (parse ['b', 'c'] (NT 'S') atnEnv))

-- Example #2 from the ALL(*) paper
parseTest3 = TestCase (assertEqual "for parse [b, d],"
                              (Just True, [])
                              (parse ['b', 'd'] (NT 'S') atnEnv))

-- Input that requires more recursive traversals of the A ATN
parseTest4 = TestCase (assertEqual "for parse [a a a a b c],"
                              (Just True, [])
                              (parse ['a', 'a', 'a', 'a', 'b', 'c'] (NT 'S') atnEnv))

parseTest5 = TestCase (assertEqual "for parse [a b a c],"
                              (Just True, [])
                              (parse ['a', 'b', 'a', 'c'] (NT 'S') atnEnv))

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
                     
                                         ([[(MIDDLE 5, 1, []), (MIDDLE 5, 2, []),(MIDDLE 5, 3, []), (MIDDLE 5, 1, [MIDDLE 1]), (MIDDLE 5, 2, [MIDDLE 1])],
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
                                        (Just True, [])
                                        (parse ['a'] (NT 'S') ambigEnv))

ambigParseTest2 = TestCase (assertEqual "for parse [a b],"
                                        (Just True, [])
                                        (parse ['a', 'b'] (NT 'S') ambigEnv))


        
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
