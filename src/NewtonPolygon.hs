{-# LANGUAGE TemplateHaskell #-}

module NewtonPolygon
  ( computeDualMono
  , computeMono
  , qualifiedMonomies
  , Monodromy(..)
  , Partition(..)
  , DualS(..)
  , DualMono(..)
  , MonoS(..)
  , Mono(..)
  , Orbit
  , Slope
  , Signature
  , name
  , bound
  , ramies
  , signature
  , sign
  , signL
  , monoA
  , monoB
  , duals
  , slopeA
  , slopeB
  , par
  , orbits
  , mods
  , slopeM
  , parM
  , monoM
  , monos
  ) where
import           Control.Lens
import           Control.Monad                  ( foldM )
import           Data.List
import qualified Data.Map                      as M
import qualified Data.MultiMap                 as MM
import           Data.Ratio
import qualified Data.Set                      as S
import           Data.Text                      ( Text )

type Orbit = S.Set Int
type Slope = Ratio Int
type Signature = [Int]

data Monodromy = Monodromy
  { _name      :: {-# UNPACK #-} !Text
  , _bound     :: {-# UNPACK #-} !Int
  , _ramies    :: [Int] -- ^ content of monodromy
  , _signature :: [Int] -- ^ signature of monodromy: len list (bound-1)
  }

instance Eq Monodromy where
  Monodromy n1 b1 r1 s1 == Monodromy n2 b2 r2 s2 =
    n1 == n2 && b1 == b2 && r1 == r2 && s1 == s2

instance Ord Monodromy where
  Monodromy _ b1 r1 _ `compare` Monodromy _ b2 r2 _ =
    b1 `compare` b2 <> r1 `compare` r2

data Partition = Partition
  { _orbits :: [Orbit]
  , _mods   :: [Int]
  }

data DualS = DualS
  { _slopeA :: [[(Slope, Int)]]
  , _slopeB :: [[(Slope, Int)]]
  , _par    :: Partition
  }

data DualMono = DualMono
  { _monoA :: Monodromy
  , _monoB :: Monodromy
  , _duals :: [DualS]
  }

data MonoS = MonoS
  { _slopeM :: [[(Slope, Int)]]
  , _parM   :: Partition
  }

data Mono = Mono
  { _monoM :: Monodromy
  , _monos :: [MonoS]
  }

$(makeLenses ''Monodromy)
$(makeLenses ''Partition)
$(makeLenses ''DualS)
$(makeLenses ''DualMono)
$(makeLenses ''MonoS)
$(makeLenses ''Mono)

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

signL :: Int -> [Int] -> Signature
signL bound ramies = sign bound ramies <$> [1 .. (bound - 1)]

qualifiedMonomies
  :: Int {-numbranchA-}
  -> Int {-numBranchB-}
  -> Int {-sumA-}
  -> Int {-sumB-}
  -> Int {-bound-}
  -> [(Monodromy, Monodromy)]
qualifiedMonomies na nb sa sb bound = do
  ra <- findRamyGen na bound (sa * bound)
  rb <- findRamyGen nb bound (sb * bound)
  let ma = Monodromy "A" bound ra (sign bound ra <$> [1 .. (bound - 1)])
      mb = Monodromy "B" bound rb (sign bound rb <$> [1 .. (bound - 1)])
  if valid ma mb then return ((,) ma mb) else []

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

computeSlopeL :: Int -> Signature -> Orbit -> [(Slope, Int)]
computeSlopeL m sig orb =
  let
    card = S.size orb
    n1   = S.findMin orb
    sup  = (sig !! (m - 1 - n1)) + (sig !! (n1 - 1))
    a = S.toDescList $ S.filter (\x -> x >= 1 && x <= sup - 1) (S.fromList sig)
    dif  = zipWith (-) (sup : a) (a ++ [0])
    s =
      (\x -> (length $ S.filter (\i -> sig !! (i - 1) >= x) orb) % card)
        <$> (sup : a)
  in
    zip s ((* card) <$> dif)

-- dual 
computeDualS :: Monodromy -> Monodromy -> [Partition] -> [DualS]
computeDualS ma mb partitions = do
  par <- partitions
  let orb = par ^. orbits
      b   = ma ^. bound
      sa  = computeSlopeL b (ma ^. signature) <$> orb
      sb  = computeSlopeL b (mb ^. signature) <$> orb
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

-- not dual: used for inputing ramies
computeMonoS :: Monodromy -> [Partition] -> [MonoS]
computeMonoS mn partitions = do
  par <- partitions
  let orb = par ^. orbits
      b   = mn ^. bound
      sd  = computeSlopeL b (mn ^. signature) <$> orb
  return (MonoS sd par)

computeMono :: Int -> [Int] -> Mono
computeMono b ramies =
  let partitions = toPartition (partitionM b)
      mn         = Monodromy "D" b ramies (signL b ramies)
  in  Mono mn (computeMonoS mn partitions)
