-- import Control.Monad
import Control.Monad.Writer

-- import Control.Monad.State

-- newtype Writer w a = Writer (a,w)

newtype MyString a = MyString {getMyString :: String}

instance Show (MyString a) where
  show (MyString s) = show s

instance Semigroup (MyString a) where
  (<>) (MyString a) (MyString b) = MyString filtered
    where
      withCrap = "(" ++ a ++ "+" ++ b ++ ")"
      filtered = [x | x <- withCrap]

instance Monoid (MyString a) where
  mempty = MyString []

fastCountDown :: Int -> Writer (MyString Int) ()
fastCountDown 0 = do
  tell . MyString . show $ 0
fastCountDown x = do
  tell . MyString . show $ x
  fastCountDown $ x -1

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s

instance Monad ST where
  -- return :: a -> ST a
  return x = S (\s -> (x, s))

  -- (>>= ) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S
      ( \s ->
          let (x, s') = (app st s)
           in app (f x) s'
      )

instance Applicative ST where
  pure x = S (\s -> (x, s))

  -- (<*>):: ST (a->b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    S (\s -> ((f x), s))

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = do
    x <- st
    S (\s -> ((f x), s))