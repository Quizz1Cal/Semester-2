type Bird = Integer
type Birds = (Bird, Bird)
data Juggler a = Juggle a | Fallen
    deriving (Eq, Show)

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

tryanddo = do
    x <- Juggle (0,0) -- Effectively the (0,0) is unpacked
    y <- addBirds x (0,1)
    addBirds y (1,1)