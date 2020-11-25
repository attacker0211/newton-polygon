{-# LANGUAGE TemplateHaskell #-}

module NewtonPolygon
  ( qualifiedMonomies
  , ppNewton
  , ppNewtonL
  ) where
import           Data.List
import qualified Data.MultiMap                 as MM
import qualified Data.Map                      as M
import           Data.Ratio
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           Control.Monad                  ( foldM )
import           Control.Lens

type Orbit = S.Set Int
type Slope = Ratio Int

data Monodromy = Monodromy
  { _name      :: {-# UNPACK #-} !Text
  , _bound     :: {-# UNPACK #-} !Int
  , _ramies    :: [Int] -- ^ content of monodromy
  , _signature :: [Int] -- ^ signature of monodromy: len list (bound-1)
  }

data Partition = Partition
  { _orbit :: [Orbit]
  , _mods  :: [Int]
  }

data DualS = DualS
  { _slopeA :: [Slope]
  , _slopeB :: [Slope]
  , _par    :: Partition
  }

data DualMono = DualMono
  { _monoA :: Monodromy
  , _monoB :: Monodromy
  , _obs   :: [DualS]
  }

$(makeLenses ''Monodromy)
$(makeLenses ''Partition)
$(makeLenses ''DualS)
$(makeLenses ''DualMono)

extractFracPart :: Int -> Int -> Double
extractFracPart denom num =
  let x = (fromIntegral num) / (fromIntegral denom)
  in  x - (fromIntegral . floor) x

findRamyGen
  :: Int {-numBranch-}
  -> Int {-bound-}
  -> Int {-sum-}
  -> [[Int]] {-ramies-}
findRamyGen nbr bound su = filter
  (\li -> sum li == su && foldl gcd bound li == 1)
  (fst <$> (foldM find ([], 0) [1 .. nbr]))
 where
  find ([], su') _ = [1 .. (bound - 1)] >>= \x -> pure ([x], su' + x)
  find (l, su') _ =
    [last l .. min (su - su') (bound - 1)] >>= \x -> pure (l ++ [x], su' + x)

sign
  :: Int {-bound-}
  -> [Int] {-ramies-}
  -> Int {-given n, range of n is [1..m-1]-}
  -> Int {-signature-}
sign bound ramies n =
  (round . sum) ((extractFracPart bound) <$> (((*) (-n)) <$> ramies)) - 1

qualifiedMonomies
  :: Int {-numbranchA-}
  -> Int {-numBranchB-}
  -> Int {-sumA-}
  -> Int {-sumB-}
  -> Int {-bound-}
  -> [(Monodromy, Monodromy)]
qualifiedMonomies na nb sa sb bound = do
  ra <- ramA
  rb <- ramB
  let ma = Monodromy "A" bound ra (sign bound ra <$> [1 .. (bound - 1)])
      mb = Monodromy "B" bound rb (sign bound rb <$> [1 .. (bound - 1)])
  if valid ma mb then d ++ [(,) ma mb] else d
 where
  ramA = findRamyGen na bound sa
  ramB = findRamyGen nb bound sb
  d    = []

valid :: Monodromy -> Monodromy -> Bool
valid monoA monoB =
  let b  = monoA ^. bound
      sa = monoA ^. signature
      sb = monoB ^. signature
  in  sum [ (sa !! x) * (sb !! (b - 2 - x)) | x <- [0 .. (b - 2)] ] == 1

{-given the bound, find possible partitioning-}
partitionM :: Int -> MM.MultiMap (S.Set Orbit) Int
partitionM b =
  let mods = filter (\x -> gcd x b == 1) [1 .. b - 1]
  in  (foldr (\x -> MM.insert (process b x [1 .. b - 1] S.empty S.empty x) x)
             MM.empty
             mods
      )
 where
  process _ _ [] orbit lorb _ =
    if orbit == S.empty then lorb else S.insert orbit lorb
  process bound modulo li orbit lorb cur = if elem cur li
    then process bound
                 modulo
                 (delete cur li)
                 (S.insert cur orbit)
                 lorb
                 ((cur * modulo) `mod` bound)
    else process bound modulo li S.empty (S.insert orbit lorb) (head li)

toPartition :: MM.MultiMap (S.Set Orbit) Int -> [Partition]
toPartition mm =
  M.foldrWithKey (\k x xs -> (Partition (S.toList k) x) : xs) [] (MM.toMap mm)

computeSlopeGen
  :: (Int -> Bool)
  -> [Int] {-signature-}
  -> Orbit
  -> Slope
computeSlopeGen f _ orb =
  let l = filter f (S.toList orb) in (length l) % (S.size orb)

computeSlopeA :: [Int] -> Orbit -> Slope
computeSlopeA sig = computeSlopeGen (\x -> sig !! (x - 1) == 1) sig

computeSlopeB :: [Int] -> Orbit -> Slope
computeSlopeB sig = computeSlopeGen (\x -> sig !! (x - 1) /= 0) sig

computeDualS :: Monodromy -> Monodromy -> [Partition] -> [DualS]
computeDualS ma mb partitions = do
  par <- partitions
  let orb = par ^. orbit
      sa  = computeSlopeA (ma ^. signature) <$> orb
      sb  = computeSlopeB (mb ^. signature) <$> orb
  return (DualS sa sb par)

computeDualMono
  :: Int {-numbranchA-}
  -> Int {-numbranchB-}
  -> Int {-sumA-}
  -> Int {-sumB-}
  -> Int {-bound-}
  -> [DualMono]
computeDualMono na nb sa sb b = do
  (ma, mb) <- qualifiedMonomies na nb sa sb b
  return (DualMono ma mb (computeDualS ma mb partitions))
  where partitions = toPartition (partitionM b)

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
        <> ppListGen (ppDualS (ma ^. name) (mb ^. name) <$> (dm ^. obs))
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
        <+> ppObs (pa ^. orbit) (ds ^. slopeA)
        <>  Pretty.hardline
        <>  pretty nb
        <>  Pretty.colon
        <+> ppObs (pa ^. orbit) (ds ^. slopeB)
        <>  Pretty.hardline

ppObs :: [Orbit] -> [Slope] -> Doc a
ppObs []  _   = Pretty.emptyDoc
ppObs _   []  = Pretty.emptyDoc
ppObs [o] [s] = Pretty.parens
  (  ppListBrac Pretty.braces (Pretty.pretty <$> (S.toList o))
  <> Pretty.comma
  <> Pretty.pretty (show s)
  )
ppObs (o : ox) (s : sx) = ppObs [o] [s] <> Pretty.comma <> ppObs ox sx

ppListBrac :: (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBrac f li = (f . Pretty.hcat) (intersperse Pretty.comma li)

ppList :: [Doc a] -> Doc a
ppList li = ppListGen li Pretty.comma

ppListGen :: [Doc a] -> Doc a -> Doc a
ppListGen li sep = (Pretty.hcat (intersperse sep li))
