import Data.List

-- Arithmetic Series
asequence :: Int -> Int -> Int -> [Int]
asequence a d x = [a+n*d | n <- [0..x]]

aseries :: Int -> Int -> Int -> String
aseries a d x = concat $ intersperse "+" $ map show $ asequence a d x

tn :: Int -> Int -> Int -> Int
tn n a d = a + (n-1)*d

sn :: Int -> Int -> Int -> Int
sn n a d = (n `div` 2) * (2*a + (n-1)*d)
--------------------------------------------------

-- Collatz Sequence
next n
	| even n = n `div` 2
	| otherwise = 3*n + 1

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x = x : collatz (next x)
--------------------------------------------------

main :: IO ()
main = do
	putStrLn $ aseries (1) (2) (100)
	putStrLn $ concat . intersperse "->" $ map show $ collatz 13