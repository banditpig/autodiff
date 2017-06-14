
-- is infinite data structure
data  Diff a = Diff a (Diff a) 

valX :: Diff a -> a
valX (Diff x _)  = x

difX :: Diff a -> Diff a
difX (Diff _ dx) = dx

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

-- diff :: Num a => Int -> (Diff a -> Diff a) -> a -> [a]
diff n f x = splitN n (f (var x))


instance Show a => Show (Diff a) where
  show (Diff x (Diff x' (Diff x'' _))) = show [x, x', x'']


