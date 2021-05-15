module Degenerate
  ( degenerate
  , epsilon
  , epsilon'
  ) where
import           Control.Lens
import qualified Data.Set                      as S
import           NewtonPolygon

degenerate :: Monodromy -> [(Monodromy, Monodromy)]
degenerate monoA =
  let a = monoA ^. ramies
      n = length a
      m = monoA ^. bound
  in  filter
        (\(monoB, _) -> (monoB ^. signature) !! 0 == 1)
        ((S.toList . S.fromList)
          [ let s = (a !! x + a !! y) `rem` m
                b = (a !! x : [a !! y, m - s])
                c =
                  [s]
                    ++ (   (a !!)
                       <$> (  [0 .. (min x y) - 1]
                           ++ [(min x y) + 1 .. (max x y) - 1]
                           ++ [(max x y) + 1 .. (n - 1)]
                           )
                       )
                monoB = Monodromy "b" m b (signL m b)
                monoC = Monodromy "c" m c (signL m c)
            in  (monoB, monoC)
          | x <- [0 .. n - 1]
          , y <- [0 .. n - 1]
          , x /= y
          , a !! x <= a !! y
          , (a !! x + a !! y) `rem` m /= 0
          ]
        )


epsilon' :: Int -> [(Monodromy, Monodromy)] -> [Int]
epsilon' bound l =
  (\(monoB, monoC) ->
      gcd (gcd bound ((monoB ^. ramies) !! 2))
          (gcd bound ((monoC ^. ramies) !! 0))
        - 1
    )
    <$> l

epsilon :: Monodromy -> [Int]
epsilon monoA = epsilon' (monoA ^. bound) (degenerate monoA)
