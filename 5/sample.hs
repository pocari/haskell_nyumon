module Sample where
import           GHC.Base                      as GB
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad
import           Data.List
import           Text.Printf
-- import           System.Random
import           System.Environment


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
--   (>>) :: m a -> m b -> m b return :: a -> m a
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

-------------------------------------------------------------------------
-- 5.1.2

type Card = Int
type Score = Int
type Hand = [Card]
type Stock = [Card]
type Player = String

drawCard :: Int -> State Stock Hand
drawCard n = do
  deck <- get
  put $ drop n deck
  return $ take n deck

gameWithState :: State Stock [(Score, Hand, Player)]
gameWithState = do
  taro    <- drawCard 5
  hanako  <- drawCard 5
  takashi <- drawCard 5
  yumi    <- drawCard 5
  return
    . reverse
    . sort
    $ [ (sum taro   , taro   , "Taro")
      , (sum hanako , hanako , "Hanako")
      , (sum takashi, takashi, "Takashi")
      , (sum yumi   , yumi   , "Yumi")
      ]

-- game :: IO ()
-- game = do
--   g <- newStdGen
--   let stock = take 20 $ randomRs (1, 50) g
--   print $ evalState gameWithState stock

-------------------------------------------------------------------------
-- 5.2
join' :: Monad m => m (m a) -> m a
join' mma = do
  ma <- mma
  ma

forTest :: IO ()
forTest = do
  forM_ [1 .. 9 :: Int] $ \s -> do
    forM_ [1 .. 9 :: Int] $ \t -> do
      putStr $ printf "%2d\t" (s * t)
    putStrLn ""

-------------------------------------------------------------------------
-- 5.4

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "error: division by zero"
safeDiv x y = Right $ x `div` y

calc :: Int -> Either String (Int, Int)
calc n = do
  x <- safeDiv 100 n
  y <- safeDiv 200 (x - 1)
  return (x, y)

safeDiv' :: Int -> Int -> Except String Int
safeDiv' x 0 = throwE $ "error: division by zero: " ++ show x ++ " / 0"
safeDiv' x y = return $ x `div` y

calc' :: Int -> Either String (Int, Int)
calc' n = runExcept $ do
  catchE
    (do
      x <- safeDiv' 100 n
      y <- safeDiv' 200 (x - 1)
      return (x, y)
    )
    (\s -> throwE $ s ++ " append hoge")

alternativeTest1 :: Either String Int
alternativeTest1 = runExcept $ throwE "hoge" <|> return 100

alternativeTest2 :: Either String Int
alternativeTest2 = runExcept $ throwE "hoge" <|> throwE " foo"

