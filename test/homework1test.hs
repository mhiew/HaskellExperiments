import Test.HUnit
import System.Exit
import Homework1

-- verify that we can force a failure
failure :: Test
failure = TestList [
    "force a failure" ~: True ~=? False
    ]

toDigitsTests = TestList [
    "negative digits return empty" ~: [] ~=? toDigits (-1),
    "zero returns empty" ~: [] ~=? toDigits 0,
    "one digit" ~: [8] ~=? toDigits 8,
    "multiple digits" ~: [1,2,3,4,5,9,8,7] ~=? toDigits 12345987
    ]

toDigitsRevTests = TestList [
    "negative digits return empty" ~: [] ~=? toDigitsRev (-1),
    "zero returns empty" ~: [] ~=? toDigitsRev 0,
    "one digit" ~: [8] ~=? toDigits 8,
    "multiple digits" ~: [7,8,9,5,4,3,2,1] ~=? toDigitsRev 12345987
    ]

doubleEveryOtherTest = TestList [
    "empty lists are not doubled" ~: [] ~=? doubleEveryOther [],
    "one item wont double" ~: [4] ~=? doubleEveryOther [4],
    "three items" ~: [1,4,3] ~=? doubleEveryOther [1,2,3],
    "four items" ~: [16,7,12,5] ~=? doubleEveryOther [8,7,6,5],
    "negative values are doubled" ~: [-16, -7, -12, 5] ~=? doubleEveryOther [-8, -7, -6, 5]
    ]

validateTest = TestList [
    True ~=? validate 4012888888881881,
    False ~=? validate 4012888888881882
    ]

hanoi3PegTest = TestList [
    "negative disks" ~: hanoi (-10) "a" "b" "c" ~?= [],
    "no disks" ~: hanoi 0 "a" "b" "c" ~?= [],
    "one disk" ~: hanoi 1 "a" "b" "c" ~?= [("a","b")],
    "two disks" ~: hanoi 2 "a" "b" "c" ~?= [("a","c"),("a", "b"),("c", "b")],
    "three disks" ~: hanoi 3 "a" "b" "c" ~?= [
        ("a","b"),("a","c"),("b","c"),  -- first 2 disks are stacked on c
        ("a","b"),                      -- last disk moved to b
        ("c","a"),("c","b"),("a","b")   -- move 2 disks on c back onto b
        ],
    "four disks" ~: hanoi 4 "a" "b" "c" ~?= [
        ("a","c"),("a","b"),("c","b"),("a","c"),("b","a"),("b","c"),("a","c"),  -- first 3 disks are stacked on c
        ("a","b"),                                                              -- last disk moved to b
        ("c","b"),("c","a"),("b","a"),("c","b"),("a","c"),("a","b"),("c","b")   -- move 3 disks on c to b 
        ]
    ]

hanoi4PegTest = TestList [
    "negative disks" ~: hanoi4 (-10) "a" "b" "c" "d" ~?= [],
    "no disks" ~: hanoi4 0 "a" "b" "c" "d" ~?= [],
    "one disk" ~: hanoi4 1 "a" "b" "c" "d" ~?= [("a","b")],
    "two disks" ~: hanoi4 2 "a" "b" "c" "d" ~?= [("a","c"),("a","b"),("c","b")],
    "three disks" ~: hanoi4 3 "a" "b" "c" "d" ~?= [("a","c"), ("a","d"),("a","b"),("d","b"),("c","b")],
    "four disks" ~: hanoi4 4 "a" "b" "c" "d" ~?= [
        ("a","b"),("a","c"),("b","c"),  -- first half stacked on c
        ("a","d"),                      -- second lesser half stacked on d
        ("a","b"),                      -- move last disk to b
        ("d","b"),                      -- move second lessed hack to b
        ("c","a"),("c","b"),("a","b")   -- move first half to b
        ],
    "five disks" ~: hanoi4 5 "a" "b" "c" "d" ~?= [
        ("a","b"),("a","c"),("b","c"),  -- first half stacked on c
        ("a","b"),("a","d"),("b","d"),  -- second lesser half stacked on d
        ("a","b"),                      -- move last disk to b
        ("d","a"),("d","b"),("a","b"),  -- move second lessed hack to b
        ("c","a"),("c","b"),("a","b")   -- move first half to b
        ]
    ]

hanoi4PegsEfficiency = TestList [
    "129 moves for 15 disks" ~: 129 ~=? length (hanoi4 15 "a" "b" "c" "d")
    ]

testSuite = test [
    --failure,
    toDigitsTests,
    toDigitsRevTests,
    doubleEveryOtherTest,
    validateTest,
    hanoi3PegTest,
    hanoi4PegTest,
    hanoi4PegsEfficiency
    ]

main :: IO ()
main = do
    counts <- runTestTT testSuite
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure