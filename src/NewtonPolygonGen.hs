module NewtonPolygonGen
  ( ppNewtonGen
  , ppNewtonGenL
  , ppNewton
  , ppNewtonL
  , ppDualMono
  ) where
import           Control.Lens
import           Data.List
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Doc
                                                , Pretty(..)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           NewtonPolygon

ppNewtonGen
  :: Int {-numbranchA-}
  -> Int {-numbranchB-}
  -> Int {-sumA-}
  -> Int {-sumB-}
  -> Int {-bound-}
  -> Doc a
ppNewtonGen na nb sa sb b =
  let l = computeDualMono na nb sa sb b
  in  "m ="
        <+> Pretty.pretty b
        <>  Pretty.hardline
        <>  ppListGen (ppDualMono <$> l) Pretty.hardline

ppNewtonGenL
  :: Int {-numbranchA-}
  -> Int {-numbranchB-}
  -> Int {-sumA-}
  -> Int {-sumB-}
  -> [Int] {-bound-}
  -> Doc a
ppNewtonGenL na nb sa sb bds =
  ppListGen ((ppNewtonGen na nb sa sb) <$> bds) Pretty.hardline

ppNewton :: Int -> Doc a
ppNewton b = ppNewtonGen 3 4 b (2 * b) b

ppNewtonL :: [Int] -> Doc a
ppNewtonL bds = ppListGen (ppNewton <$> bds) Pretty.hardline

ppDualMono :: DualMono -> Doc a
ppDualMono dm =
  let ma = (dm ^. monoA)
      mb = (dm ^. monoB)
  in  ppMono ma
        <> Pretty.hardline
        <> ppMono mb
        <> Pretty.hardline
        <> ppListGen (ppDualS (ma ^. name) (mb ^. name) <$> (dm ^. duals))
                     Pretty.hardline

ppMono :: Monodromy -> Doc a
ppMono mono =
  pretty (mono ^. name)
    <+> Pretty.equals
    <+> ppRamy (mono ^. ramies)
    <>  Pretty.comma
    <+> ppSig (mono ^. signature)
 where
  ppRamy ramies = ppListBrac Pretty.parens (Pretty.pretty <$> ramies)
  ppSig sig = ppListBrac Pretty.brackets (Pretty.pretty <$> sig)

ppDualS :: Text -> Text -> DualS -> Doc a
ppDualS na nb ds =
  let pa = ds ^. par
  in  "p"
        <+> Pretty.equals
        <+> ppList (Pretty.pretty <$> (pa ^. mods))
        <>  Pretty.hardline
        <>  pretty na
        <>  Pretty.colon
        <+> ppObs (pa ^. orbits) (ds ^. slopeA)
        <>  Pretty.hardline
        <>  pretty nb
        <>  Pretty.colon
        <+> ppObs (pa ^. orbits) (ds ^. slopeB)
        <>  Pretty.hardline

ppObs :: [Orbit] -> [[(Slope, Int)]] -> Doc a
ppObs [] _  = Pretty.emptyDoc
ppObs _  [] = Pretty.emptyDoc
ppObs [o] [s] =
  ppListBrac Pretty.parens (Pretty.pretty <$> (S.toList o))
    <> Pretty.colon
    <> Pretty.space
    <> ppListBracSep (Pretty.semi <> Pretty.space)
                     Pretty.brackets
                     (ppSlope <$> s)
ppObs (o : ox) (s : sx) = ppObs [o] [s] <> Pretty.hardline <> ppObs ox sx

ppSlope :: (Slope, Int) -> Doc a
ppSlope (s, mul) = Pretty.parens
  (Pretty.pretty (show s) <> Pretty.comma <> Pretty.space <> Pretty.pretty mul)

ppListBracSep :: Doc a -> (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBracSep sep f li = (f . Pretty.hcat) (intersperse sep li)

ppListBrac :: (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBrac f li = ppListBracSep Pretty.comma f li

ppList :: [Doc a] -> Doc a
ppList li = ppListGen li Pretty.comma

ppListGen :: [Doc a] -> Doc a -> Doc a
ppListGen li sep = (Pretty.hcat (intersperse sep li))
