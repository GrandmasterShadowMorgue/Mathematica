
-- Arithmetic Series
aseries :: Int -> Int -> Int -> [Int]
asequence a d x = zipWith (+) (replicate x a)([n*d | n <- [0..x]])

main :: IO ()
main = print $ asequence 1 2 100