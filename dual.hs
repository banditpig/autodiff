
import Vectors
data Dual a = Dual a a deriving (Show)

instance Functor (Dual) where 
  fmap f (Dual x x') = Dual (f x) (f x')

instance Applicative Dual where
  -- pure :: a -> f a
  -- (<*>) :: f (a -> b) -> f a -> f b
  pure x = Dual x x
  Dual f f'  <*> Dual x x' =  Dual (f x) (f' x')

instance Monad Dual where
  -- (>>=) :: m a -> (a -> m b) -> m b 
   (Dual x x') >>= f = f x

instance Num a =>  Num (Dual a) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (Dual x dx) + (Dual y dy) = Dual (x + y) (dx + dy)
    (Dual x dx) - (Dual y dy) = Dual (x - y) (dx - dy)
    (Dual x dx) * (Dual y dy) = Dual (x * y) (x * dy + y * dx)
    abs (Dual x dx)           = Dual (abs x) (dx * (signum x))
    signum (Dual x dx)        = Dual (signum x) 0
    fromInteger n             = Dual (fromInteger n) 0

instance Fractional a => Fractional (Dual a) where
  Dual u du / Dual v dv = Dual (u / v) (( du * v - u * dv)/(v * v))
  fromRational x = Dual (fromRational x) 0

instance (Fractional a, Floating a) => Floating (Dual a) where
  pi                = Dual pi 0
  exp   (Dual x dx) = Dual (exp x)   (dx * exp x)
  log   (Dual x dx) = Dual (log x)   (dx / x)
  sin   (Dual x dx) = Dual (sin x)   (dx * cos x)
  cos   (Dual x dx) = Dual (cos x)   (- dx * sin x)
  asin  (Dual x dx) = Dual (asin x)  (dx / (sqrt(1 - x ** 2)))
  acos  (Dual x dx) = Dual (acos x)  (- dx / (sqrt(1 - x ** 2)))
  atan  (Dual x dx) = Dual (atan x)  (dx / (1 + x ** 2))
  sinh  (Dual x dx) = Dual (sinh x)  (dx * cosh x)
  cosh  (Dual x dx) = Dual (cosh x)  (dx * sinh x)
  asinh (Dual x dx) = Dual (asinh x) (dx / (sqrt(1 + x ** 2)))
  acosh (Dual x dx) = Dual (acosh x) (dx / (sqrt(x ** 2 - 1)))
  atanh (Dual x dx) = Dual (atanh x) (dx / (1 - x ** 2))
derivDual :: Dual a -> (a, a)
derivDual (Dual x x') = (x, x')

--derivfx :: Num a => (Dual a -> Dual a) -> a -> (a, a)
derivfx f x = derivDual . f $ Dual x 1

pDx, pDy, pDz :: (Num a) => (Dual a -> Dual a -> Dual a -> t) -> a -> a -> a -> t
pDx f x y z = f (Dual x 1) (Dual y 0) (Dual z 0) 
pDy f x y z = f (Dual x 0) (Dual y 1) (Dual z 0) 
pDz f x y z = f (Dual x 0) (Dual y 0) (Dual z 1) 


grad :: (Num a, Num a, Num a) => (Dual a -> Dual a-> Dual a -> Dual a) -> a -> a -> a -> (a, a, a)
grad f x y z =   (x', y', z' ) where Dual _ x' = pDx f x y z
                                     Dual _ y' = pDy f x y z
                                     Dual _ z' = pDz f x y z

gradV :: (Dual Scalar -> Dual Scalar -> Dual Scalar -> Dual Scalar)  -> Scalar -> Scalar -> Scalar -> Vector
gradV f x y z = V  (grad f x y z)

g x y z = (exp 1)**(x + y + z) + ((x*y*z)/sqrt(x + y + z))
h x y z = foldr (\_ a -> (x + y + z)  + atan(a + ( g (x+a) (y+a) (z+a)))) (x + y + z) [1..1000]
