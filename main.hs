
-- Arithmetic Series
aseries :: Int -> Int -> Int -> [Int]
aseries a d x = . zipWith (+) (replicate x a)([n*d | n <- [0..x]])

main :: IO ()
main = print $ aseries 1 2 100