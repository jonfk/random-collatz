import System.Environment
import System.Random

collatz 1 = [1]
collatz n
        | even n = n:collatz (n `div` 2)
        | odd n = n:collatz (3*n + 1)

main = do
  args <- getArgs
  if length args > 0 then
      do
        let num = read $ head args :: Integer
        print $ collatz num
  else
      do
        gen <- getStdGen
        let
            (randNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
            listRandom = take randNum $ randoms gen :: [Integer]
            largeRandNum = foldl (\x y -> x * y) 1 listRandom
        print largeRandNum
        print $ collatz . abs $ largeRandNum
        print largeRandNum
