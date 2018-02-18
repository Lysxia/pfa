{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Control.Concurrent.STM as STM
import Data.Functor.Classes
import Data.Kind
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Typeable
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Counterexamples as QCC
import Test.StateMachine

import Data.PFA.Kumar
import Data.PFA.Internal.Log.Class
import qualified Data.PFA.Internal.Log.Chunks as LogC

data Ref a = Ref Int (Opaque a)

instance Eq (Ref a) where
  Ref i _ == Ref j _ = i == j

instance Ord (Ref a) where
  compare (Ref i _) (Ref j _) = compare i j

instance Show (Ref a) where
  show (Ref i _) = "(Ref " ++ show i ++ " _)"

unRef :: Reference Concrete (Ref a) -> a
unRef (Reference (Concrete (Ref _ (Opaque a)))) = a

newtype Counter = Counter (IO Int)

newCounter :: IO Counter
newCounter = do
  r <- STM.newTVarIO 0
  return . Counter . STM.atomically $ do
    i <- STM.readTVar r
    STM.writeTVar r $! i+1
    return i

ref :: Counter -> a -> IO (Ref a)
ref (Counter count) a = do
  i <- count
  return (Ref i (Opaque a))

data Action log x (v :: Type -> Type) :: Type -> Type where
  New :: Int -> x -> Action log x v (Ref (PFA log x))
  Get :: Reference v (Ref (PFA log x)) -> Int -> Action log x v x
  Set
    :: Reference v (Ref (PFA log x))
    -> Int -> x -> Action log x v (Ref (PFA log x))

instance HTraversable (Action log x) where
  htraverse _ (New n x) = pure (New n x)
  htraverse f (Get v i) = (\w -> Get w i) <$> htraverse f v
  htraverse f (Set v i x) = (\w -> Set w i x) <$> htraverse f v

instance HFoldable (Action log x)
instance HFunctor (Action log x)

instance Constructors (Action log x) where
  constructor New{} = Constructor "New"
  constructor Get{} = Constructor "Get"
  constructor Set{} = Constructor "Set"

  nConstructors _ = 3

instance (Show1 v, Show x) => Show1 (Action log x v) where
  liftShowsPrec _ _ p act = showParen (p > 10) $
    case act of
      New n x ->
        showString "New "
          . showsPrec 11 n . showString " "
          . showsPrec 11 x
      Get v i ->
        showString "Get "
          . showsPrec 11 v . showString " "
          . showsPrec 11 i
      Set v i x ->
        showString "Set "
          . showsPrec 11 v . showString " "
          . showsPrec 11 i . showString " "
          . showsPrec 11 x

instance (Show1 v, Show x, Show a) => Show (Action log x v a) where
  showsPrec = showsPrec1

semantics
  :: Logging log
  => Action log x Concrete r -> ReaderT Counter IO r
semantics (New n x) = do
  count <- ask
  lift (newIO n x >>= ref count)
semantics (Get v i) = lift (getIO (unRef v) i)
semantics (Set v i x) = do
  count <- ask
  lift (setIO (unRef v) i x >>= ref count)

newtype Model log x v = Model [(Reference v (Ref (PFA log x)), IntMap x)]
  deriving Show

initModel :: Model log x v
initModel = Model []

modelInsert
  :: Reference v (Ref (PFA log x))
  -> IntMap x -> Model log x v -> Model log x v
modelInsert r a (Model m) = Model ((r, a) : m)

modelLookup
  :: Ord1 v
  => Reference v (Ref (PFA log x))
  -> Model log x v
  -> Maybe (IntMap x)
modelLookup r (Model m) = lookup r m

precondition :: Model log x Symbolic -> Action log x Symbolic a -> Bool
precondition _ (New n _) = 0 <= n
precondition (Model m) (Get v i) =
  case lookup v m of
    Just v' -> 0 <= i && i < IntMap.size v'
    Nothing -> False
precondition (Model m) (Set v i _) =
  case lookup v m of
    Just v' -> 0 <= i && i < IntMap.size v'
    Nothing -> False

transition :: Ord1 v => Model log x v -> Action log x v r -> v r -> Model log x v
transition m (New n x) r =
  modelInsert (Reference r) (IntMap.fromList (zip [0 .. n-1] (repeat x))) m
transition m (Get _ _) _ = m
transition m (Set v i x) r =
  case modelLookup v m of
    Just v' -> modelInsert (Reference r) (IntMap.insert i x v') m
    Nothing -> error "invalid state"

postcondition :: Eq x => Model log x Concrete -> Action log x Concrete r -> r -> Bool
postcondition _ (New _ _) _ = True
postcondition m (Get v i) r =
  case modelLookup v m of
    Just v' -> IntMap.lookup i v' == Just r
    Nothing -> False
postcondition _ (Set _ _ _) _ = True

retryM :: Monad m => m (Maybe a) -> m a
retryM g = do
  a <- g
  case a of
    Nothing -> retryM g
    Just a -> return a

generator
  :: (QC.Arbitrary x, Show x, Typeable log, Typeable x)
  => Model log x Symbolic -> QC.Gen (Untyped (Action log x))
generator (Model m) = retryM $ QC.frequency
      [ (1,    genNew)
      , (getW, genGet)
      , (setW, genSet)
      ]
  where
    genNew = do
      QC.NonNegative n <- QC.arbitrary
      x <- QC.arbitrary
      return (Just (Untyped (New n x)))
    getW = 3 * setW
    setW = length m
    genGet = forV $ \v i -> return (Just (Untyped (Get v i)))
    genSet = forV $ \v i -> do
      x <- QC.arbitrary
      return (Just (Untyped (Set v i x)))
    forV go = do
      (v, v') <- QC.elements m
      let n = IntMap.size v'
      if n == 0 then
        return Nothing
       else do
        i <- QC.choose (0, IntMap.size v' - 1)
        go v i

shrinker :: QC.Arbitrary x => Action log x v r -> [Action log x v r]
shrinker (New i x) = [New i' x' | (i', x') <- QC.shrink (i, x)]
shrinker (Get v i) = [Get v i' | i' <- QC.shrink i]
shrinker (Set v i x) = [Set v i' x' | (i', x') <- QC.shrink (i, x)]

sm
  :: (Logging log, Typeable log, Eq x, QC.Arbitrary x, Show x, Typeable x)
  => StateMachine (Model log x) (Action log x) (ReaderT Counter IO)
sm =
  stateMachine
    generator
    shrinker
    precondition
    transition
    postcondition
    initModel
    semantics
    (\run -> newCounter >>= runReaderT run)

prop_sequential :: QCC.PropertyOf (Program (Action LogC.Log Int))
prop_sequential = monadicSequentialC sm $ \prog -> do
  (hist, _, res) <- runProgram sm prog
  prettyProgram sm hist $
    checkActionNames prog (res QC.=== Ok)

main = QCC.quickCheck prop_sequential
