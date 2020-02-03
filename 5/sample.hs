module Sample where
import           GHC.Base                      as GB
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.Identity
import           Data.List
import           Text.Printf
-- import           System.Random
import           System.Environment
import           Control.Monad.Trans.Reader
import           Control.Monad.ST
import           Data.STRef
import           Data.Array.ST

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

-------------------------------------------------------------------------
-- 5.5

-- Readerモナドを実装してみる
newtype Reader' r a = Reader' { runReader' :: r -> a }

instance Monad (Reader' r) where
  -- return :: a -> Reader r a
  return x = Reader' $ \_ -> x
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  r >>= f = Reader'
    $ \arg -> let a = runReader' r arg in let r2 = f a in runReader' r2 arg

instance Applicative (Reader' r) where
  -- pure :: a -> Reader' r a
  pure = return

  -- (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
  readerF <*> readerArg = do
    f   <- readerF
    arg <- readerArg
    return $ f arg

instance Functor (Reader' r) where
  -- fmap :: (a -> b) -> Reader' r a -> Reader' r b
  fmap f r = do
    a <- r
    return $ f a

ask' :: Reader' r r
ask' = Reader' id

local' :: (r -> r) -> Reader' r a -> Reader' r a
local' f r = do
  source <- ask'
  let source' = f source
  Reader' $ \_ -> runReader' r source'

asks' :: (r -> a) -> Reader' r a
asks' f = do
  r <- ask'
  Reader' $ \_ -> f r


readRound :: Reader' Double Int
readRound = round <$> ask'

data PowerEnv = PowerEnv {powEnergy :: !Double, powSaveMode :: !Bool}
              deriving Show

consume :: Reader' PowerEnv Double
consume = do
  energy   <- asks' powEnergy
  saveMode <- asks' powSaveMode
  let result = if saveMode then energy / 10.0 else energy
  return result

testrun :: PowerEnv -> Double
testrun env = flip runReader' env $ do
  cons1      <- consume
  cons2      <- consume
  consOthers <- local' (\e -> e { powSaveMode = True }) $ do
    cons3 <- consume
    cons4 <- consume
    return $ cons3 + cons4
  return $ cons1 + cons2 + consOthers

-------------------------------------------------------------------------
-- 5.6

procCount :: Integer
procCount = runST $ do
  n <- newSTRef 0
  forM_ [1 .. 10] $ \i -> do
    modifySTRef n (+ i)
  readSTRef n

doubleArray :: [Double]
doubleArray = runST $ do
  arr <- newListArray (0, 4) [1 .. 5] :: ST s (STUArray s Int Double)
  x   <- readArray arr 2
  writeArray arr 2 (x * 10.0)
  getElems arr

-------------------------------------------------------------------------
-- 5.8
testMaybeT :: IO ()
testMaybeT = do
  maybeValue <- runMaybeT $ do
    let env = lookupEnv "USER"
    a <- lift env
    case a of
      (Just x) -> lift $ print $ "a is Just " ++ x
      Nothing  -> lift $ print $ "a is Nothing"
    MaybeT env
  print maybeValue

-- https://qiita.com/7shi/items/4408b76624067c17e933#%E5%9E%8B%E8%A1%A8%E8%A8%98

return' :: a -> State s a
return' x = StateT $ \s -> Identity (x, s)

runState' :: State s a -> s -> (a, s)
runState' st s = runIdentity $ runStateT st s

hoge :: IO ()
hoge = do
  let st = return' (1 :: Int)
  print $ runState' st ()
