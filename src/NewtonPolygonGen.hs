module NewtonPolygonGen
  ( ppNewtonGen
  , ppNewtonGenL
  , ppNewtonMonoGen
  , ppNewton
  , ppNewtonL
  , ppDualMono
  , ppMonodromy
  ) where
import           Control.Lens
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Doc
                                                , Pretty(..)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           GenUtils
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

ppNewtonMonoGen :: Int -> [Int] -> Doc a
ppNewtonMonoGen b ramies =
  let l = computeMono b ramies
  in  "m =" <+> Pretty.pretty b <> Pretty.hardline <> ppMono l

ppDualMono :: DualMono -> Doc a
ppDualMono dm =
  let ma = (dm ^. monoA)
      mb = (dm ^. monoB)
  in  ppMonodromy ma
        <> Pretty.hardline
        <> ppMonodromy mb
        <> Pretty.hardline
        <> ppListGen (ppDualS (ma ^. name) (mb ^. name) <$> (dm ^. duals))
                     Pretty.hardline

ppMono :: Mono -> Doc a
ppMono mono =
  let mn = mono ^. monoM
  in  ppMonodromy mn
        <> Pretty.hardline
        <> ppListGen (ppMonoS (mn ^. name) <$> (mono ^. monos)) Pretty.hardline

ppMonodromy :: Monodromy -> Doc a
ppMonodromy mono =
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
        <>  ppMonoSS na pa (ds ^. slopeA)
        <>  ppMonoSS nb pa (ds ^. slopeB)

ppMonoS :: Text -> MonoS -> Doc a
ppMonoS nd ds =
  let pa = ds ^. parM
  in  "p"
        <+> Pretty.equals
        <+> ppList (Pretty.pretty <$> (pa ^. mods))
        <>  Pretty.hardline
        <>  ppMonoSS nd pa (ds ^. slopeM)

ppMonoSS :: Text -> Partition -> [[(Slope, Int)]] -> Doc a
ppMonoSS name pa slope =
  pretty name <> Pretty.colon <+> ppObs (pa ^. orbits) slope <> Pretty.hardline

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
