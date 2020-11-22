module Main where
import           NewtonPolygon

main :: IO ()
main = print (ppNewtonL [3 .. 100])
