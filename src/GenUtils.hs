module GenUtils
  ( ppListBracSep
  , ppListBrac
  , ppList
  , ppListGen
  ) where
import           Data.List
import           Data.Text.Prettyprint.Doc      ( Doc )
import qualified Data.Text.Prettyprint.Doc     as Pretty

ppListBracSep :: Doc a -> (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBracSep sep f li = (f . Pretty.hcat) (intersperse sep li)

ppListBrac :: (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBrac f li = ppListBracSep Pretty.comma f li

ppList :: [Doc a] -> Doc a
ppList li = ppListGen li Pretty.comma

ppListGen :: [Doc a] -> Doc a -> Doc a
ppListGen li sep = (Pretty.hcat (intersperse sep li))
