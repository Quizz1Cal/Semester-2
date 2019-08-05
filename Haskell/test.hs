type Bird = Integer
type Birds = (Bird, Bird)
data Juggler a = Juggle a | Fallen

instance Functor Juggler where
    fmap f Fallen = Fallen
    fmap f (Juggle x) = Juggle (f x)

instance Applicative Juggler where
    pure = Juggle
    Fallen <*> _ = Fallen
    _ <*> Fallen = Fallen
    (Juggle f) <*> (Juggle x) = Juggle (f x) 

instance Monad Juggler where
    return = Juggle
    Juggle birds >>= f = f birds
    Fallen >>= f = Fallen
    fail _ = Fallen

addBirds :: Birds -> Birds -> Juggler Birds
addBirds (a,b) (x,y)
    | a+x-b-y > 3 || a+x-b-y < -3 = Fallen
    | otherwise    = pure (a+x, b+y)

factorial :: Int -> Int
factorial n
    | n <= 1 = 1
    | otherwise = n * factorial (n-1)