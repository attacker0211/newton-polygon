module DegenerateGen
  ( ppDeg
  ) where
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Doc
                                                , Pretty(..)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           Degenerate
import           GenUtils
import           NewtonPolygon
import           NewtonPolygonGen

ppDeg :: Int -> [Int] -> Doc a
ppDeg bound ramies =
  let quals = degenerate (Monodromy "A" bound ramies (signL bound ramies))
      eps   = epsilon' bound quals
  in  ppListGen
        (zipWith
          (\(monoB, monoC) e ->
            "e"
              <+> Pretty.equals
              <+> pretty e
              <>  Pretty.hardline
              <>  ppMonodromy monoB
              <>  Pretty.hardline
              <>  ppMonodromy monoC
              <>  Pretty.hardline
          )
          quals
          eps
        )
        (Pretty.hardline)


