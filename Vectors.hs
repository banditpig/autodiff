{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}

module Vectors 
  ( Scalar
  , Vector(..)
  , XYZ
  , (^+^)
  , (^-^)
  , (^*)
  , (*^)
  , (><)
  , (>.<)
  , (^/)
  , i
  , j
  , k
  , origin
  , normalise
  , neg
  , mag
  , rotateXY

  ) where
import Text.Printf

type Scalar = Float

type XYZ = (Scalar, Scalar, Scalar)

newtype Vector = V { xyz :: XYZ} deriving  (Eq)
instance Show Vector where
  show (V (x, y, z) ) = "(x:" ++ p x ++  " y:" ++ p y ++ " z:" ++ p z ++ ")" where 
   p d = printf "%.2f" d :: String

instance Num Vector where
  -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
  (+) = (^+^)
  (-) = (^-^)
  (*) = (><)
  abs = id
  fromInteger n = V (fromIntegral n, fromIntegral n,fromIntegral n)
  signum = normalise

instance Monoid Vector where
  mempty = origin
  -- mappend :: Vector -> Vector -> Vector
  mappend = (^+^)


-- Define operators using  functions defined later
infixl 6 ^+^
(^+^) :: Vector -> Vector -> Vector
(^+^) = vAdd

infixl 6 ^-^
(^-^) :: Vector -> Vector -> Vector
(^-^) = vSub

infixl 7 *^
(*^) :: Scalar -> Vector -> Vector
(*^) = sMul

infixl 7 ^*
(^*) :: Vector -> Scalar -> Vector
(^*) = sMul'

infixl 7 ^/
(^/) :: Vector -> Scalar  -> Vector
(^/) = sDiv

infixl 7 >.<
(>.<) :: Vector -> Vector -> Scalar
(>.<) = dot

infixl 7 ><
(><) :: Vector -> Vector -> Vector
(><) = cross

--  The 'traditional' i, j, k 
i, j, k, origin :: Vector
i = V (1, 0, 0)
j = V (0, 1, 0)
k = V (0, 0, 1)
origin = V (0, 0, 0)

-- Normalise 
normalise :: Vector -> Vector
normalise (V (0, 0, 0)) = V (0, 0, 0) 
normalise v = sDiv v (mag v) 

-- negate vector
neg :: Vector -> Vector
neg = mapVec ((-1) *)

-- Magnitude of a vector
mag :: Vector -> Scalar
mag = sqrt . sumVec . mapVec (^2)

-- rotate in the X Y plane.
rotateXY :: Scalar -> Vector -> Vector
rotateXY theta (V (x, y, z ))  = V (x', y', z) where
  x' = x * cos theta - y * sin theta
  y' = x * sin theta + y * cos theta
-- ----------------------------------------------------------
-- Map a function over the internal tuple
mapVec :: (Scalar -> Scalar) -> Vector -> Vector
mapVec f (V (x, y, z)) = V (f x, f y, f z)

-- Just sum the tuple entries
sumVec :: Vector -> Scalar
sumVec (V (x, y, z)) = x + y + z

-- Apply a scalar function to x,y,z components of each vector
zipWithVec :: (Scalar -> Scalar -> Scalar) -> Vector -> Vector -> Vector
zipWithVec f (V (x, y, z)) (V (x', y', z')) = V (f x x', f y y', f z z')

-- These are fairly obvious.  The functions defined above
-- make these functions simpler and cleaner.

-- Just adding/subtracting  the corresponding components, 
-- so zipWithVec using (+) or (-)
vAdd :: Vector -> Vector -> Vector
vAdd = zipWithVec (+)

vSub :: Vector -> Vector -> Vector
vSub = zipWithVec (-)

-- Scalar then Vector - just map (*) over tuple entries
sMul :: Scalar -> Vector -> Vector
sMul s = mapVec (* s)

-- or Vector then Scalar
sMul' :: Vector -> Scalar -> Vector
sMul' = flip sMul

sDiv :: Vector -> Scalar ->  Vector
sDiv v s = mapVec (/ s) v

-- dot product
dot :: Vector -> Vector -> Scalar
dot x = sumVec . zipWithVec (*) x

-- Cross product - is there a neater way?
-- note also that b×a=−a×b  and a×a=0 - will use QuickCheck
cross :: Vector -> Vector -> Vector
cross (V (u1, u2, u3)) (V (v1, v2, v3)) =
  V (u2 * v3 - u3 * v2, u3 * v1 - u1 * v3, u1 * v2 - u2 * v1)


