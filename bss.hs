{-# LANGUAGE NumDecimals #-}
import Math.NumberTheory.Primes
import Foreign (newArray0, Bits (xor))
import Control.Monad (replicateM)
import Data.Foldable (foldl')

bigInt :: Integer
bigInt = 10e9

p = until (\x -> x `mod` 4 == 3) (unPrime . nextPrime) bigInt
q = until (\x -> x `mod` 4 == 3) (unPrime . nextPrime) p
n = p * q

nwd :: Integer -> Integer -> Integer
nwd a 0 = a
nwd a b = nwd b (a `mod` b)

lsb :: Integer -> Integer
lsb x = x `mod` 2

randomCoprime :: Integer -> Integer
randomCoprime 0 = 1 
randomCoprime n = until (\x -> nwd x n == 1) (\x -> x - 1)  n

randomCoprimes :: Int -> Integer -> [Integer]
randomCoprimes x n = take x (iterate randomCoprime n)

randomBits :: [Integer] -> [Integer]
randomBits = map (\ x -> lsb (x ^ 2) `mod` n)

bitsToNumber :: [Integer] -> Integer 
bitsToNumber = foldl' (\acc x -> acc * 2 + x) 0

countOnes :: [Integer] -> Int
countOnes = foldl' (\acc x -> if x == 1 then acc + 1 else acc) 0

bits = randomBits (randomCoprimes 20000 n)
bitsCount = countOnes bits 

number = bitsToNumber bits

main :: IO ()
main = do
  print "Random number"
  print number
  print "Number of ones"
  print bitsCount
