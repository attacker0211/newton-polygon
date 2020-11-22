module NewtonPolygon
  ( qualifiedMonomies
  , ppNewton
  , ppNewtonL
  , ppList
  ) where
import           Data.List
import qualified Data.MultiMap                 as MM
import           Data.Ratio
import qualified Data.Set                      as S
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

-- data Mono = M
--   { nbranches :: {-# UNPACK #-} !Int -- ^ Number of branches (default: 3)
--   , sum       :: {-# UNPACK #-} !Int -- ^ Sum of ramies 
--   , bound     :: {-# UNPACK #-} !Int -- ^ Bound of each rami
--   , ramies    :: [Int] -- ^ Content of monodromy a
--   }
--   deriving Generic

type Bound = Int
type Signature = Int
type Mono = [Int] -- ^ Mono is a list of three integers (x_1, x_2, x_3) 
type MonoSig = (Mono, [Signature])
type Moduli = Int
type Orbit = S.Set Int -- ^ Integers that lie in the same orbit mod bound
type Slope = Ratio Int
type Newton = (Orbit, Slope)

extractFracPart :: Int -> Int -> Double
extractFracPart denom num =
  let x = (fromIntegral num) / (fromIntegral denom)
  in  x - (fromIntegral . floor) x

findMonoA :: Bound -> Int -> [Mono] -- ^ find triples that satisfied x_1 <= x_2 <= x3 <= bound, gcd(x_1, x_2, x_3, m) = 1 and x_1 + x_2 + x_3 = sum
findMonoA bound sum = filter
  (\[x1, x2, x3] -> x1 + x2 + x3 == sum && gcd x1 (gcd x2 (gcd x3 bound)) == 1)
  [ [x1, x2, x3]
  | x1 <- [1 .. bound]
  , x2 <- [x1 .. (max bound (sum - x1))]
  , x3 <- [x2 .. (max bound (sum - x1 - x2))]
  ]

findMonoB :: Bound -> Int -> [Mono]
findMonoB bound sum = filter
  (\[x1, x2, x3, x4] ->
    x1 + x2 + x3 + x4 == sum && gcd x1 (gcd x2 (gcd x3 (gcd x4 bound))) == 1
  )
  [ [x1, x2, x3, x4]
  | x1 <- [1 .. bound]
  , x2 <- [x1 .. (max bound (sum - x1))]
  , x3 <- [x2 .. (max bound (sum - x1 - x2))]
  , x4 <- [x2 .. (max bound (sum - x1 - x2 - x3))]
  ]

sign :: Bound -> Mono -> Int -> Signature -- ^ range of n is [1..m-1] 
sign bound monomies n =
  (round . sum) ((extractFracPart bound) <$> (((*) (-n)) <$> monomies)) - 1

qualifiedMonomies :: Bound -> [(MonoSig, MonoSig)]
qualifiedMonomies bound = do
  a <- ma
  b <- mb
  let sa = sign bound a <$> [1 .. (bound - 1)] -- ^ compute signature function for each A
      sb = sign bound b <$> [1 .. (bound - 1)] -- ^ compute signature function for each B 
  if valid bound sa sb then d ++ [((a, sa), (b, sb))] else d
 where
  ma = findMonoA bound bound -- ^ list of monomies A = [ a1, a2, a3 ] such that a1 <= a2 <= a3 <= m and a1 + a2 + a3 = m
  mb = findMonoB bound (2 * bound) -- ^ list of monomies B = [ b1, b2, b3 ] such that b1 <= b2 <= b3 <= m and b1 + b2 + b3 = 2m
  d  = []

valid :: Bound -> [Signature] -> [Signature] -> Bool -- ^ check condition f(A,1)f(B,m-1)=1 && f(a,x)f(B,m-x)=0
valid bound sa sb =
  if sum [ (sa !! x) * (sb !! (bound - 2 - x)) | x <- [0 .. (bound - 2)] ] == 1
    then True
    else False

partitionOrb :: Bound -> [(S.Set Orbit, Moduli)]
partitionOrb bound = do
  moduli <- filter (\x -> gcd x bound == 1) [1 .. (bound - 1)]
  zip
    (return (process bound moduli [1 .. (bound - 1)] (S.empty) S.empty moduli))
    (return moduli)

computeSlopeA :: [Signature] -> Orbit -> Newton
computeSlopeA sig = computeSlopeGen (\x -> sig !! (x - 1) == 1) sig

computeSlopeB :: [Signature] -> Orbit -> Newton
computeSlopeB sig = computeSlopeGen (\x -> sig !! (x - 1) /= 0) sig

computeSlopeGen :: (Int -> Bool) -> [Signature] -> Orbit -> Newton
computeSlopeGen f sig orb =
  let l = filter f (S.toList orb) in (orb, (length l) % (S.size orb))

slope
  :: MonoSig
  -> MonoSig
  -> [(S.Set Orbit, [Moduli])]
  -> [([Moduli], [Newton], [Newton])]
slope msa msb partitions = do
  (orbits, moduli) <- partitions
  let newtona = computeSlopeA (snd msa) <$> (S.toList orbits)
      newtonb = computeSlopeB (snd msb) <$> (S.toList orbits)
  return (moduli, newtona, newtonb)

computeNewton
  :: Bound -> [(MonoSig, MonoSig, [([Moduli], [Newton], [Newton])])]
computeNewton bound = do
  (a, b) <- monomies
  let sab = slope a b partitions
  return (a, b, sab)
 where
  partitions = toMM (partitionOrb bound)
  monomies   = qualifiedMonomies bound

toMM :: [(S.Set Orbit, Moduli)] -> [(S.Set Orbit, [Moduli])]
toMM a = (MM.assocs . MM.fromList) a

process
  :: Bound -> Moduli -> [Int] -> Orbit -> S.Set Orbit -> Int -> S.Set Orbit
process _ _ [] orbit lorb _ =
  if orbit == S.empty then lorb else S.insert orbit lorb
process bound moduli li orbit lorb cur = if elem cur li
  then process bound moduli (delete cur li) (S.insert cur orbit) lorb e
  else process bound moduli li S.empty (S.insert orbit lorb) (head li)
  where e = (cur * moduli) `mod` bound

ppNewton :: Bound -> Doc a
ppNewton bound =
  let l = computeNewton bound
  in  "m ="
        <+> Pretty.pretty bound
        <>  Pretty.hardline
        <>  ppListGen (ppElem <$> l) Pretty.hardline

ppNewtonL :: [Bound] -> Doc a
ppNewtonL bounds = ppListGen (ppNewton <$> bounds) Pretty.hardline

ppElem :: (MonoSig, MonoSig, [([Moduli], [Newton], [Newton])]) -> Doc a
ppElem (a, b, nt) =
  "A ="
    <+> ppMonoSig a
    <>  Pretty.hardline
    <>  "B ="
    <+> ppMonoSig b
    <>  Pretty.hardline
    <>  ppListGen (ppMObs <$> nt) Pretty.hardline
    <>  Pretty.hardline

ppMObs :: ([Moduli], [Newton], [Newton]) -> Doc a
ppMObs (mods, nta, ntb) =
  "p ="
    <+> ppList (Pretty.pretty <$> mods)
    <>  Pretty.hardline
    <>  "a"
    <>  Pretty.colon
    <+> ppList (ppObs <$> nta)
    <>  Pretty.hardline
    <>  "b"
    <>  Pretty.colon
    <+> ppList (ppObs <$> ntb)

ppMonoSig :: (Mono, [Signature]) -> Doc a
ppMonoSig (mono, sig) =
  ppListBrac Pretty.parens (Pretty.pretty <$> mono)
    <> Pretty.comma
    <> ppListBrac Pretty.brackets (Pretty.pretty <$> sig)

ppObs :: Newton -> Doc a
ppObs (orb, slope) = Pretty.parens
  (  ppListBrac Pretty.braces (Pretty.pretty <$> (S.toList orb))
  <> Pretty.comma
  <> Pretty.pretty (show slope)
  )

ppListBrac :: (Doc a -> Doc a) -> [Doc a] -> Doc a
ppListBrac f li = (f . Pretty.hcat) (intersperse Pretty.comma li)

ppList :: [Doc a] -> Doc a
ppList li = ppListGen li Pretty.comma

ppListGen :: [Doc a] -> Doc a -> Doc a
ppListGen li sep = (Pretty.hcat (intersperse sep li))
