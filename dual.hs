


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
  exp   (Dual x dx) = Dual (exp x) (dx * exp x)
  log   (Dual x dx) = Dual (log x) (dx / x)
  sin   (Dual x dx) = Dual (sin x) (dx * cos x)
  cos   (Dual x dx) = Dual (cos x) (- dx * sin x)
  asin  (Dual x dx) = Dual (asin x) (dx / (sqrt(1 - x ** 2)))
  acos  (Dual x dx) = Dual (acos x) (- dx / (sqrt(1 - x ** 2)))
  atan  (Dual x dx) = Dual (atan x) (dx / (1 + x ** 2))
  sinh  (Dual x dx) = Dual (sinh x) (dx * cosh x)
  cosh  (Dual x dx) = Dual (cosh x) (dx * sinh x)
  asinh (Dual x dx) = Dual (asinh x) (dx / (sqrt(1 + x ** 2)))
  acosh (Dual x dx) = Dual (acosh x) (dx / (sqrt(x ** 2 - 1)))
  atanh (Dual x dx) = Dual (atanh x) (dx / (1 - x ** 2))

derivDual :: Dual a -> (a, a)
derivDual (Dual x x') = (x, x')

derivfx :: Num a => (Dual a -> Dual a) -> a -> (a, a)
derivfx f x = derivDual . f $ Dual x 1

