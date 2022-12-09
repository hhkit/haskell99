import Test.HUnit

pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode arr = [(length x, head x) | x <- pack arr]

data Encoded a = Multiple Int a | Single a
instance Eq t => Eq (Encoded t) where
    Single a == Single b = a == b
    (==) (Multiple m a) (Multiple n b) = a == b && m == n
    (==) _ _ = False
instance Show t => Show (Encoded t) where 
    show (Single a) = show a
    show (Multiple m a) = show m ++ " " ++ show a

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified arr = [ if x == 1 then Single y else Multiple x y | (x,y) <- encode arr ]

testEncodeModified = "encodeModified" ~: 
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e'] ~=? encodeModified "aaaabccaadeeee"

decodeModified :: [Encoded a] -> [a]
decodeModified = foldr (\x acc -> case x of
    Single a -> a:acc
    Multiple m b -> replicate m b ++ acc
    ) [] 

testDecodeModified = "decodeModified" ~:
    "aaaabccaadeeee" ~=? decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr (\x acc -> case acc of
    [] -> [Single x]
    (Single y:ys) -> if x == y then Multiple 2 x:ys else Single x : Single y : ys
    (Multiple m y:ys) -> if x == y then Multiple (m+1) x:ys else Single x : Multiple m y : ys) []

testEncodeDirect = "encodeDirect" ~: 
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e'] ~=? encodeDirect "aaaabccaadeeee"

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

testDuplicate = "duplicate" ~: [1,1,2,2,3,3] ~=? dupli [1,2,3]


repli :: [a] -> Int -> [a]
-- repli arr m = foldr ((++) . replicate m) [] arr
repli arr m = concatMap (replicate m) arr

testReplicate = "replicate" ~: test [
    [1,1,2,2,3,3] ~=? repli [1,2,3] 2,
    "aaabbbccc" ~=? repli "abc" 3
    ]

dropEvery :: [a] -> Int -> [a]
dropEvery arr m = map fst $ filter (\(_,n) -> n /= m) (zip arr (cycle [1..m]))

testDrop = "dropEvery" ~: test [
    "abdeghk" ~=? dropEvery "abcdefghik" 3
    ]

split :: [a] -> Int -> ([a], [a])
split m 0 = ([], m)
split (x:xs) n = let (f, s) = split xs (n-1) in
    (x:f, s)

testSplit = "split" ~: ("abc", "defghik") ~=? split "abcdefghik" 3

slice :: [a] -> Int -> Int -> [a]
slice arr 1 0 = []
slice (x:xs) 1 end = x:slice xs 1 (end-1)
slice (x:xs) beg end = slice xs (beg-1) (end-1)
slice arr _ _ = []

testSlice = "slice" ~: "cdefg" ~=? slice ['a','b','c','d','e','f','g','h','i','k'] 3 7

rotate :: [a] -> Int -> [a]
rotate arr n = if n >= 0 then let (f,s) = split arr n in s ++ f
    else let (f,s) = split arr (length arr + n) in s ++ f

testRotate = "rotate" ~: test [
    "defghabc" ~=? rotate ['a','b','c','d','e','f','g','h'] 3,
    "ghabcdef" ~=? rotate ['a','b','c','d','e','f','g','h'] (-2)
    ]

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt m (x:xs) = let (rem, rest) = removeAt (m-1) xs in (rem, x:rest)

testRemoveAt = "removeAt" ~: ('b', "acd") ~=? removeAt 2 "abcd"

main = do
    runTestTT (test [
        testEncodeModified,
        testDecodeModified,
        testEncodeDirect,
        testDuplicate,
        testReplicate,
        testDrop,
        testSplit,
        testSlice,
        testRotate,
        testRemoveAt
        ])