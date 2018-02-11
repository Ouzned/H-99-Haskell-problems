-- 1. Find last element of a list
myLast :: [a] -> a
myLast = head . reverse

-- 2. Find the last but one element of a list
myButLast = head . tail . reverse

-- 3. Find the K'th element of a list. The first element in the list is number 1. 
elementAt xs n 
    | length xs < n = error("Index out of bounds")
    | otherwise = last $ take n xs

-- 4. Find the number of elements of a list. 
myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0

myLength' :: [a] -> Int
myLength' = sum . map (\_ -> 1)

-- 5. Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' =  foldl (\acc x -> x : acc) []

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x) 
isPalindrome xs = all (\(a,b) -> a == b) (zip xs $ reverse xs)

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' (x:xs) = x == last xs && isPalindrome' (init xs)

-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldMap flatten xs

-- 8. Eliminate consecutive duplicates of list elements
compress [] = []
compress [x] = [x]
compress (x:xs) 
    | x == head(xs) = compress xs 
    | otherwise = x : compress xs

compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

-- 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
pack [] = []
pack (x:xs) = 
    let (first, rest) = span (== x) xs
    in (x : first) : pack rest

-- 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding 
-- data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number 
-- of duplicates of the element E. 
encode :: Eq a => [a] -> [(Int,a)] 
encode = map (\x -> (length x, head x)) . pack 

encode' :: Eq a => [a] -> [(Int,a)] 
encode' xs = [(length x, head x) | x <- pack xs]

-- 11. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements 
-- with duplicates are transferred as (N E) lists. 
data ListElem a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [ListElem a]
encodeModified = map encode . filter (not . null) . pack 
    where encode l@(x:xs) 
            | len == 1 = Single x
            | otherwise = Multiple len x
            where len = length l

-- 12. Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. 
decodeModified :: [ListElem a] -> [a]
decodeModified = concatMap decodeHelper
    where 
        decodeHelper (Multiple n x) = replicate n x
        decodeHelper (Single x) = [x]

-- 13. Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the 
-- duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 
encodeDirect [] = []
encodeDirect (x:xs)  
    | first == [] = Single x : encodeDirect rest
    | otherwise = (Multiple len x) : encodeDirect rest
    where 
        (first, rest) = span (== x) xs
        len = length first + 1

-- 14. Duplicate the elements of a list
dupli :: [a] -> [a]
dupli = concatMap (\n -> replicate 2 n)

-- 15. Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs

-- 16. Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery l n =
    let dropHelper [] _ = []
        dropHelper (x:xs) 1 = dropHelper xs n
        dropHelper (x:xs) k = x : dropHelper xs (k - 1) 
    in dropHelper l n

-- 17.  Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a],[a])
split xs n = 
    let (first,second) = break (\(x,y) -> x == n+1) $ zip [1..] xs
    in (map snd first, map snd second)

split' xs n = (take n xs, drop n xs)

-- 18. Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list 
-- (both limits included). Start counting the elements with 1. 
slice :: [a] -> Int -> Int -> [a]
slice xs i k | i > 1 && k >= i = take (k - i + 1) $ drop (i - 1) xs

slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _  = []
slice' (x:xs) i k
    | i > 1      = slice' xs (i - 1) (k - 1)
    | k < 1      = []
    | otherwise  = x:slice' xs (i - 1) (k - 1)

-- 19. Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate xs n 
    | n > 0 = chain n leftRotate $ xs
    | otherwise = chain (abs n) rightRotate $ xs 
    where 
        chain k f = foldl (.) id (replicate k f)
        leftRotate (x:xs) = xs ++ [x]
        rightRotate = reverse . leftRotate . reverse
          
-- 20. Remove the K'th element from a list
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
