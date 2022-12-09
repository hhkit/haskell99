import Test.HUnit

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

testMyLast = "myLast" ~: test [
    4 ~=? myLast [1,2,3,4],
    'z' ~=? myLast "xyz"
    ]

myButLast :: [a] -> a 
myButLast = (!! 1) . reverse

testMyButLast = "myButLast" ~: test [
    3 ~=? myButLast [1,2,3,4],
    'y' ~=? myButLast "xyz"
    ]

elementAt :: [a] -> Int -> a 
elementAt (a:_) 1 = a
elementAt (x:xs) n = elementAt xs (n-1)

testElementAt = "elementAt" ~: test [
    2 ~=? elementAt [1,2,3] 2,
    'e' ~=? elementAt "haskell" 5
    ]

myLength :: [a] -> Int 
myLength = foldr (const (+1)) 0

testMyLength = "myLength" ~: test [
    3 ~=? myLength [123, 456, 789],
    13 ~=? myLength "Hello, world!"
    ]

myReverse :: [a] -> [a]
myReverse = foldl (\x acc -> acc:x) []

testMyReverse = "myReverse" ~: test [ 
    "!amanap ,lanac a ,nalp a ,nam A" ~=? myReverse "A man, a plan, a canal, panama!",
    [4,3,2,1] ~=? myReverse [1,2,3,4]
    ]

isPalindrome :: Eq a => [a] -> Bool 
isPalindrome arr = and (zipWith (==) arr (myReverse arr))

testIsPalindrome = "isPalindrome" ~: test [ 
    False ~=? isPalindrome [1,2,3],
    True ~=? isPalindrome "madamimadam",
    True ~=? isPalindrome [1,2,4,8,16,8,4,2,1]
    ]

data NestedList a = Show a => Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
-- flatten (List x) = foldr (++) [] (map flatten x)
-- flatten (List x) = foldr ((++) . flatten) [] x
-- flatten (List x) = concat (map flatten x)
flatten (List x) = concatMap flatten x

testFlatten = "flatten" ~: test [
    [5] ~=? flatten (Elem 5),
    [1,2,3,4,5] ~=? flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]),
    [] ~=? flatten (List []::NestedList Int)
    ]

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
  | x == y = compress (x:xs) 
  | otherwise = x : compress (y:xs) 
compress (x:_) = [x]

testCompress = "compress" ~: test [ 
    "abcade" ~=? compress "aaaabccaadeeee"
    ]

pack :: Eq a => [a] -> [[a]]
pack = foldr (\x y -> case y of
    y:ys -> if x == head y then (x:y):ys else [x]:y:ys
    _ -> [[x]]
    ) ([]::[[a]])

testPack = "pack" ~: test [
    ["aaaa", "b", "cc", "aa", "d", "eeee"] ~=? pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e'],
    ["aaaa", "b", "cc", "aa", "d", "eeee"] ~=? pack "aaaabccaadeeee"
    ]

encode :: Eq a => [a] -> [(Int, a)]
-- encode arr = map (\x -> (myLength x, head x)) (pack arr)
encode arr = [(myLength x, head x) | x <- pack arr]

testEncode = "encode" ~: test [
     [(4,'a'), (1,'b'), (2,'c'), (2, 'a'), (1,'d'), (4, 'e')] ~=? encode "aaaabccaadeeee"
    ]

main = do
    runTestTT (test [
        testMyLast
        , testMyButLast
        , testElementAt
        , testMyLength
        , testMyReverse
        , testIsPalindrome
        , testFlatten
        , testCompress
        , testPack
        , testEncode
        ])