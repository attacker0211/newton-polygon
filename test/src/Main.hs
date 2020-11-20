module Main where
import           NewtonPolygon
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

main :: IO ()
main = print (Pretty.hcat (ppNewton <$> [1 .. 20]))
