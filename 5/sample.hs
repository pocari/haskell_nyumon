module Sample where
import           GHC.Base

data Maybe' a = Nothing' | Just' a
              deriving Show

-- *Sample> :i Functor
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)


-- *Sample> :i Applicative
-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   {-# MINIMAL pure, ((<*>) | liftA2) #-}

-- liftA2の方で定義したことないので試してみる
instance Applicative Maybe' where
  -- pure :: a -> Maybe' a
  pure x = Just' x

  -- GHC.Base.liftA2 :: (a -> b -> c) -> Maybe' a -> Maybe' b -> Maybe' c
  liftA2 _ Nothing'  _         = Nothing'
  liftA2 _ _         Nothing'  = Nothing'
  liftA2 f (Just' x) (Just' y) = Just' (f x y)


-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   fail :: String -> m a
--   {-# MINIMAL (>>=) #-}
instance Monad Maybe' where
  -- (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing' >>= _ = Nothing'
  Just' x  >>= f = f x


-------------------------------------------------------------------------
-- 5.1.2

type Category = String
type Name = String
type Price = Integer
type Menu = [(Category, [(Name, Price)])]
type Item = (Category, Name, Price)

menu :: Menu
menu =
  [ ("Foods" , [("Hamburger", 120), ("FrenchFries", 100)])
  , ("Drinks", [("Cola", 80), ("Tea", 100)])
  ]

getItemWithoutMonad :: Menu -> Category -> Name -> Maybe Item
getItemWithoutMonad m c n = case lookup c m of
  Just items -> case lookup n items of
    Just price -> Just (c, n, price)
    _          -> Nothing
  _ -> Nothing


getItem :: Menu -> Category -> Name -> Maybe Item
getItem m c n = do
  items <- lookup c m
  price <- lookup n items
  return (c, n, price)

