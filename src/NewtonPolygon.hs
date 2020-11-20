module NewtonPolygon
  ( qualifiedMonomies
  , ppNewton
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
type Moduli = Int
type Orbit = S.Set Int -- ^ Integers that lie in the same orbit mod bound
type Slope = Ratio Int
type Newton = (Orbit, Slope)

extractFracPart :: Int -> Int -> Double
extractFracPart denom num =
  let x = (fromIntegral num) / (fromIntegral denom)
  in  x - (fromIntegral . floor) x

findMonomies :: Bound -> Int -> [Mono] -- ^ find triples that satisfied x_1 <= x_2 <= x3 <= bound, gcd(x_1, x_2, x_3, m) = 1 and x_1 + x_2 + x_3 = sum
findMonomies bound sum = filter
  (\[x1, x2, x3] -> x1 + x2 + x3 == sum && gcd x1 (gcd x2 (gcd x3 bound)) == 1)
  [ [x1, x2, x3]
  | x1 <- [1 .. bound]
  , x2 <- [x1 .. (max bound (sum - x1))]
  , x3 <- [x2 .. (max bound (sum - x1 - x2))]
  ]

sign :: Bound -> Mono -> Int -> Signature -- ^ range of n is [1..m-1] 
sign bound monomies n =
  (round . sum) ((extractFracPart bound) <$> (((*) (-n)) <$> monomies)) - 1

qualifiedMonomies :: Bound -> [((Mono, [Signature]), (Mono, [Signature]))]
qualifiedMonomies bound = do
  a <- ma
  b <- mb
  let sa = sign bound a <$> [1 .. (bound - 1)] -- ^ compute signature function for each A
      sb = sign bound b <$> [1 .. (bound - 1)] -- ^ compute signature function for each B 
  if valid bound sa sb then d ++ [((a, sa), (b, sb))] else d
 where
  ma = findMonomies bound bound -- ^ list of monomies A = [ a1, a2, a3 ] such that a1 <= a2 <= a3 <= m and a1 + a2 + a3 = m
  mb = findMonomies bound (2 * bound) -- ^ list of monomies B = [ b1, b2, b3 ] such that b1 <= b2 <= b3 <= m and b1 + b2 + b3 = 2m
  d  = []

valid :: Bound -> [Signature] -> [Signature] -> Bool -- ^ check condition f(A,1)f(B,m-1)=1 && f(a,x)f(B,m-x)=0
valid bound sa sb =
  if (sa !! 0)
     *  (sb !! (bound - 2))
     == 1
     && (and
          [ (sa !! x) * (sb !! (bound - 2 - x)) == 0 | x <- [1 .. (bound - 3)] ]
        )
  then
    True
  else
    False

partitionOrb :: Bound -> [(S.Set Orbit, Moduli)]
partitionOrb bound = do
  moduli <- filter (\x -> gcd x bound == 1) [1 .. (bound - 1)]
  zip
    (return (process bound moduli [1 .. (bound - 1)] (S.empty) S.empty moduli))
    (return moduli)


computeSlope :: (Mono, [Signature]) -> Orbit -> (Orbit, Slope)
computeSlope (_, sig) orbit =
  let l = filter (\x -> sig !! (x - 1) == 1) lorbit
  in  (orbit, (length l) % (length lorbit))
  where lorbit = S.toList orbit

computeNewton
  :: Bound
  -> [ ( [Moduli]
       , ((Mono, [Signature]), [(Orbit, Slope)])
       , ((Mono, [Signature]), [(Orbit, Slope)])
       )
     ]
computeNewton bound = do
  (a     , b     ) <- qualifiedMonomies bound
  (orbits, moduli) <- toMM (partitionOrb bound)
  return
    ( moduli
    , (a, computeSlope a <$> (S.toList orbits))
    , (b, computeSlope b <$> (S.toList orbits))
    )

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
        <>  (Pretty.hcat) (ppElem <$> l)
        <>  Pretty.hardline

ppElem
  :: ( [Moduli]
     , ((Mono, [Signature]), [Newton])
     , ((Mono, [Signature]), [Newton])
     )
  -> Doc a
ppElem x@(mods, moa, mob) =
  ppMono x
    <>  "p ="
    <+> ppList (pretty <$> mods)
    <>  Pretty.colon
    <>  Pretty.hardline
    <>  "A"
    <>  Pretty.colon
    <+> ppList (ppObs <$> (snd moa))
    <>  Pretty.hardline
    <>  "B"
    <>  Pretty.colon
    <+> ppList (ppObs <$> (snd mob))
    <>  Pretty.hardline

ppMono
  :: ( [Moduli]
     , ((Mono, [Signature]), [Newton])
     , ((Mono, [Signature]), [Newton])
     )
  -> Doc a
ppMono (_, moa, mob) =
  "A ="
    <+> ppMonoSig (fst moa)
    <>  Pretty.comma
    <+> "B = "
    <>  ppMonoSig (fst mob)
    <>  Pretty.hardline


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
ppList li = (Pretty.hcat (intersperse Pretty.comma li))

