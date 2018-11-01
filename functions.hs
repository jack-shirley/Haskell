fh :: [a] -> [a]
fh xs = if (myLength xs `mod` 2) == 0 then take (myLength xs `div` 2) xs else error "Odd list"

sh :: [a] -> [a]
sh xs = if (myLength xs `mod` 2) == 0 then drop (myLength xs `div` 2) xs else error "Odd list"

shuffle :: Int -> [a] -> [a]
shuffle 0 xs = xs
shuffle n xs = shuffle (n-1) (riffle (fh xs) (sh xs))

-- Problem 11
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects y = [x | z <- [1..y], x <- (factors z), (sum ((factors x)) - z) == z]

-- Problem 12
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n z = [z | y <- [1..n], y <= n]



myTestList =
  TestList [
    "fibonacci" ~: fibonacci 4 ~=? 3

    , "myProduct" ~: myProduct [1..10] ~=? 3628800

    , "flatten 1" ~: flatten [[]::[Int]] ~=? []
    , "flatten 2" ~: flatten [[]::[Int], [], []] ~=? []
    , "flatten 3" ~: flatten [[1], [2, 3, 4], [], [5, 6]] ~=? [1, 2, 3, 4, 5, 6]

    , "myLength" ~: myLength [1, 2, 3] ~=? 3

    , "quicksort 1" ~: quicksort [3, 2, 5, 1, 6] ~=? [1,2,3,5,6]
    , "quicksort 2" ~: quicksort "howdy" ~=? "dhowy"

    , "isElement 1" ~: (isElement 'c' "abcd") ~=? True
    , "isElement 2" ~: (isElement 'e' "abcd") ~=? False

    ]

main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          exitWith (codeGet errs fails)

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
