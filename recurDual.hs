-- https://en.wikipedia.org/wiki/Quotient_rule
-- https://calculus.subwiki.org/wiki/Product_rule_for_higher_derivatives

data Triple a = T a a a deriving (Show)

instance Fractional a => Fractional (Triple a ) where
  fromRational n = T (fromRational n) 0 0
  (T g g' g'') / (T h h' h'') = T (g / h) ((g * h' - h * g')/ h * h) secDiff where
    secDiff = ( 2*h'*(g*h' - h*g') - h*(g*h'' - h*g'')) / (h * h * h)
instance Num a =>  Num (Triple a) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (T x x' x'') + (T y y' y'') = T (x + y)  (x' + y')  (x'' + y'') 
   
     -- d (u*v) = u dv + v du (x * dy + y * dx)
    (T x x' x'') * (T y y' y'') = T (x * y) (x * y' + y * x')  (x * y'' + 2 * y' * x' + y * x'')
    abs (T x x' x'')            = T (abs x) (x' * (signum x)) (x''* (signum x'))
    signum (T x x' x'')         = T (signum x) 0 0
    fromInteger n               = T (fromInteger n) 0 0




diffTriple (T x x' x'') = (x, x', x'')
d2 f x = diffTriple . f $ T x 1 0


-- is infinite data structure
data  Diff a = Diff a (Diff a) 
valX :: Diff a -> a
valX (Diff x _)  = x

difX :: Diff a -> Diff a
difX (Diff _ dx) = dx

var :: Num a => a -> Diff a
var x = Diff x 1


instance Num n => Num (Diff n) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (Diff x dx) + (Diff y dy) = Diff (x + y) (dx + dy)
    (Diff x dx) - (Diff y dy) = Diff (x - y) (dx - dy)
    -- d (u*v) = u dv + v du
    x@(Diff x1 dx1) * y@(Diff y1 dy1) = Diff (x1 * y1) (x * dy1 + y * dx1)
    abs (Diff x dx) = Diff (abs x) (abs dx)
    signum (Diff x dx) = Diff (signum x) 0
    fromInteger x = Diff (fromInteger x) 0

instance Fractional n => Fractional (Diff n) where
  fromRational n = Diff (fromRational n) 0
  x@(Diff x1 dx1) / y@(Diff y1 dy1) = Diff (x1 / y1) ((x * dy1 - y * dx1)/ y^2)
  recip (Diff x x') = ip where 
    ip   = Diff (recip x) (-x' * ip * ip)




instance Eq a => Eq (Diff a) where (Diff x _) == (Diff y _) = x == y
instance Ord a => Ord (Diff a) where 
    compare (Diff x _)  (Diff y _) = compare x y



splitN :: Int -> Diff a -> [a]
splitN 0 _ = []
splitN n diffx = x : splitN (n - 1) diffx' where
    (x, diffx')  = split diffx
    split (Diff x diffs) = (x, diffs)

diff :: Num a => Int -> (Diff a -> Diff a) -> a -> [a]
diff n f x = splitN n (f (var x))

diff1  = diff 1 
diff2  = diff 2


-- get upto the nth derivative value of f at x in the range
diffRng :: Num a => Int -> (Diff a -> Diff a) -> [a] -> [[a]]
diffRng _ _ [] = []
diffRng n f (x:xs) = diff n f x : diffRng n f xs

gx x = 1 / ( x * x )
-- diffRng 
fx :: (Num a, Fractional a) => a -> a
fx x  = x^3
instance Show a => Show (Diff a) where
  show (Diff x (Diff x' (Diff x'' _))) = show [x, x', x'']

-- findZero f = iterate go
-- where go xn = let (fxn,f’xn) = diff’ f xn
-- in xn - fxn / f’xn

--findZero :: (Num a, Fractional a) => ( a ->  a) -> [a]
--findZero :: (Num a, Fractional a) => (Diff a -> Diff a) -> a -> [a]
findZero f = iterate go  where
    go  xn =  (xn - fxn / fxn') where 
        (fxn:fxn':[]) = diff2 f xn 
    -- go  xn = let (fxn:fxn':[]) = diff 2 f xn in (xn - fxn / fxn')







