-- https://en.wikipedia.org/wiki/Quotient_rule
-- https://calculus.subwiki.org/wiki/Product_rule_for_higher_derivatives

-- is infinite data structure
data  Diff a = Diff a (Diff a) 

instance Num n => Num (Diff n) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (Diff x dx) + (Diff y dy) = Diff (x + y) (dx + dy)
    (Diff x dx) - (Diff y dy) = Diff (x - y) (dx - dy)
    -- d (u*v) = u dv + v du
    x@(Diff x1 dx1) * y@(Diff y1 dy1) = Diff (x1 * y1) (x * dy1 + y * dx1)
    abs (Diff x dx) = Diff (abs x) (abs dx)
    signum (Diff x dx) = Diff (signum x) 0
    fromInteger x = Diff (fromInteger x) 0

instance (Fractional n) => Fractional (Diff n) where
  -- # MINIMAL fromRational, (recip | (/)) 
  fromRational n = Diff (fromRational n) 0
  x@(Diff x1 dx1) / y@(Diff y1 dy1) = Diff (x1 / y1) ((y * dx1  - x * dy1)/ y^2)



-- helper from the paper
dlift :: Num a => [a -> a] -> Diff a -> Diff a

dlift (f : f') p@(Diff x x') = Diff (f x) (x' * dlift f' p)

instance (Fractional a, Floating a) => Floating (Diff a) where
  pi                  = Diff pi 0
  exp (Diff x dx)     = res where res = Diff (exp x) (dx * res)
  log d@(Diff x dx)   = Diff (log x) (dx / d)
  sin (Diff x dx)     = dlift (cycle [sin, cos, negate . sin, negate . cos]) (Diff x dx)
  cos (Diff x dx)     = dlift (cycle [cos, negate . sin, negate . cos, sin]) (Diff x dx)
  asin d@(Diff x dx)  = Diff (asin x) ( dx / sqrt(1 - d*d))
  acos d@(Diff x dx)  = Diff (acos x) (-dx / sqrt(1 - d*d))
  atan d@(Diff x dx)  = Diff (atan x) ( dx / (d*d  - 1))
  sinh d@(Diff x dx)  = (exp d - exp (-d)) / 2
  cosh d@(Diff x dx)  = (exp d + exp (-d)) / 2
  asinh d@(Diff x dx) = Diff (asinh x) (dx / (sqrt(1 + d*d )))
  acosh d@(Diff x dx) = Diff (acosh x) (dx / (sqrt(d*d  - 1)))
  atanh d@(Diff x dx) = Diff (atanh x) (dx / (1 - d*d))




diff :: Num a => Int -> (Diff a -> Diff a) -> a -> [a]
diff n f x = splitN n (f ( Diff x 1)) where
  splitN :: Int -> Diff a -> [a]
  splitN 0 (Diff x _) = [x] 
  splitN n diffx = x : splitN (n - 1) diffx' where
    (x, diffx')  = split diffx
    split (Diff x diffs) = (x, diffs)

diff2 :: Num a => (Diff a -> Diff a) -> a -> [a]
diff2 = diff 2

-- get upto the nth derivative value of f at x in the range
diffRng :: Num a => Int -> (Diff a -> Diff a) -> [a] -> [[a]]
diffRng _ _ [] = []
diffRng n f (x:xs) = diff n f x : diffRng n f xs

gx x = 1 / ( x * x )
-- diffRng 
fx :: (Num a, Fractional a) => a -> a
fx x  = x^3

instance Show a => Show (Diff a) where
  show (Diff x (Diff x' (Diff x'' _))) = 
    show ("f=" ++ (show x) ++ ", f'=" ++ (show x') ++ ", f''=" ++ (show x''))

  --show [x, x', x'']
-- instance Show a => Show (Diff a) where
--   show (Diff x (Diff x' (Diff x'' _))) = show [x, x', x'']

-- findZero f = iterate go
-- where go xn = let (fxn,f’xn) = diff’ f xn
-- in xn - fxn / f’xn

--findZero :: (Num a, Fractional a) => ( a ->  a) -> [a]
--findZero :: (Num a, Fractional a) => (Diff a -> Diff a) -> a -> [a]
findZero f = iterate go  where
    go  xn =  (xn - fxn / fxn') where 
        (fxn:fxn':[]) = diff2 f xn 
    -- go  xn = let (fxn:fxn':[]) = diff 2 f xn in (xn - fxn / fxn')







