
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-type-defaults
  -fno-warn-missing-signatures
  -Werror
  #-}

module Main where

import Lubeck.FRP
import System.IO
import System.Console.ANSI
import BasePrelude hiding (Signal)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
-- import Lib
import Linear.Affine
import Linear.V2
import Data.Functor.Contravariant
import Data.Profunctor

-- newtype Comp   a b = Comp   (Events a -> Signal b)
-- newtype Widget a b = Widget (Signal a -> Events b)
--
-- instance Profunctor Comp where
--   dimap f g (Comp c) = Comp $ \e -> fmap g $ c $ fmap f e
--   lmap  f   (Comp c) = Comp $ \e -> c $ fmap f e
--   rmap  f   (Comp c) = Comp $ \e -> fmap f $ c e
--
-- instance Profunctor Widget where
--   dimap f g (Widget c) = Widget $ \e -> fmap g $ c $ fmap f e
--   lmap  f   (Widget c) = Widget $ \e -> c $ fmap f e
--   rmap  f   (Widget c) = Widget $ \e -> fmap f $ c e


-- Stick a monad in here?
-- Compre halogen, reflex et al
newtype Comp   v a b = Comp   (Events a -> (Signal v, Signal b))
newtype Widget v a b = Widget (Signal a -> (Signal v, Events b))

instance Profunctor (Comp v) where
  dimap f g (Comp c) = Comp $ \e -> second (fmap g) $ c $ fmap f e

instance Profunctor (Widget v) where
  dimap f g (Widget c) = Widget $ \e -> second (fmap g) $ c $ fmap f e

-- mapHtmlWidget :: (r -> r2) -> WidgetT r a b -> WidgetT r2 a b
mapViewW :: (t -> v) -> Widget t t1 t2 -> Widget v t1 t2
mapViewW f (Widget w) = Widget $ \e -> first (fmap f) $ w $ e

mapViewC :: (t -> v) -> Comp t t1 t2 -> Comp v t1 t2
mapViewC f (Comp w) = Comp $ \e -> first (fmap f) $ w $ e

{-
For JSON viewer:

  { foo : ...
  , bar : { foo : ...
          ,*bar : ...
          , baz : [...]
          }
  , baz : [...]
  }



  Value -> Ev Key -> FRP (Signal Screen)
    Signal closes over original value and state:
      - 0 or 1 selected keys (i.e. a String value)
      - Which keys are opened, and their view
      - Note that incoming events (move up/down, open/close)
        only pertains to ONE key in the whole tree at a time.
        I.e. the component will only react to events if it has a selection AND that selection is collapsed

-}

-- Happens at most once. TODO proper type
type Future a = Events a


-- Like JSON, but only strings and objects, and lazy
data Value = Val (Map String Value) | Str String
  deriving (Show)

-- Infinite tree
exValue :: Value
exValue = Val (Map.fromList [("fin", Str "x"),("inf",exValue)])

exValue2 = Val (Map.fromList [("a",exValue2),("b", Str "x"),("c",exValue2)])

exValueFin = Val (Map.fromList [("fin", Str "x"),("fin2",Str "y")])


isUp, isDown, isLeft, isRight :: Key -> Bool
[isUp, isDown, isLeft, isRight] = undefined


data Action = MoveUp | MoveDown | MoveLeft | MoveRight | ToggleSelected
  deriving (Show, Eq, Ord)

{-
Widget renders value in a collapsed/uncollapsed state, one element being "selected"
If "active" it responds to up/down, changing selection, to left/right by moving selection into a sub-element or back, and to
Uncollapsed element are child views, calling this function recursively.
Note that the passed value may be infinite, meaning that we should not activate subelements until necessary.
  In fact, each time a sub-element is untoggled/closed, it make sense to "kill" it and reactivate it the next time it is toggled/opened again.

This function is good for top-level:
    :: Value                -- value to render
    -> Events Action        -- commands for the whole thing, may be routed to subelements
    -> FRP (Signal Screen)  -- screen to render + activation of state

Internally/for recursive call we need slightly more:
    :: Value  -- (sub-)value to render
    -> Events Action -- so we can move around in the subvalue
    -> FRP (Signal Screen, Future ()) -- subvalue allocates its own state on demand, and renders to a screen. The future indicates we haved moved out of the value.
-}

data Sel = Outside | Local Int | Nested Int
  deriving (Eq, Ord, Show)

data ToggleCommand = Open | Close | Toggle
  deriving (Eq, Ord, Show)


counterApp :: Events Key -> FRP (Signal Screen)
counterApp e = do
  countS <- accumS 0 (fmap (const succ) e)
  pure $ fmap (\n -> screenFromList $ replicate n "x") countS


explorerApp :: Events Key -> FRP (Signal Screen)
explorerApp e = do
  renderVal exValue2 (filterJust $ fmap g e)
  where
    g (LetterKey '\r') = Just ToggleSelected
    g (LetterKey ' ') = Just ToggleSelected
    g (LetterKey 'w') = Just MoveUp
    g (LetterKey 's') = Just MoveDown
    g (LetterKey 'd') = Just MoveRight
    g (LetterKey 'a') = Just MoveLeft
    g _ = Nothing


renderVal                       :: Value
                                -> Events Action
                                -> FRP (Signal Screen)
renderVal x e = do
  (u,e2) <- newEvent
  r <- renderVal2 False x e e2
  sendTo u ()
  pure (fst r)


-- TODO coloured output for selection

renderVal2                    :: Bool
                              -- ^ Allow moving left (i.e. not true for top node)
                              -> Value
                              -> Events Action
                              -> Events ()
                              -- ^ Accept selection
                              -> FRP (Signal Screen, Future ())
renderVal2 allowLeftMove (Str x) ev _             =
  pure $ (pure $ screenFromList [x], fmap (const ()) $ Lubeck.FRP.filter (== MoveRight) ev)
renderVal2 allowLeftMove (Val theValueMap) ev acc = do

  -- --------------------------------------------------------------------------------
  -- STATE
  (selU,      selS      :: Signal Sel)                                     <- newSignalA Outside
  (subCompsU, subCompsS :: Signal (Map String (Signal Screen, Future ()))) <- newSignalA mempty

  -- --------------------------------------------------------------------------------
  -- EVENT HANDLING
  (currentSubCompS :: Sink Action, currentSubCompE) <- newEvent -- undefined
  (tellSubCompN  :: Sink Int,      tellSubCompE)    <- newEvent -- undefined

  let curKey :: Signal (Maybe String) = flip fmap selS  $ \s -> case s of
        Local n -> Just $ safeIndex (Map.keys theValueMap) n
        Nested n -> Just $ safeIndex (Map.keys theValueMap) n
        _ -> Nothing
  let curLocalKey :: Signal (Maybe String) = flip fmap selS  $ \s -> case s of
        Local n -> Just $ safeIndex (Map.keys theValueMap) n
        _ -> Nothing

  -- TODO the joinE causes BAD performance degradations. Come up with something better.
  let subCompToldUs :: Events Int = joinE $ updates $ fmap (mconcat . zipWith (\n fut -> fmap (const n) fut) [0..] . fmap snd . toList) subCompsS

  let foo1 :: String -> Events Action = \n -> filterJust $ snapshotWith (\k v -> if k == Just n then Just v else Nothing) (current curKey) currentSubCompE
  let foo2 :: String -> Events ()     = \k -> filterJust $ fmap (\n -> if safeIndex (Map.keys theValueMap) n == k then Just () else Nothing ) tellSubCompE

  (tellSuperComp :: Sink (), superCompTold :: Future ()) <- newEvent

  let doWithSelected :: ToggleCommand -> Int -> IO () = \tc n -> do
          let k = safeIndex (Map.keys theValueMap) n
          case Map.lookup k theValueMap of
            Nothing -> error "Impossible"
            Just nestedVal -> do
              let evs :: Events Action = foo1 k
              let evAs :: Events () = foo2 k
              -- subscribeEvent evs $ Sink $ \x -> putStr "evs: " >> putStr k >> putStr " " >> print x
              -- subscribeEvent ev  $ Sink $ \x -> putStr "ev : " >> putStr k >> putStr " " >> print x
              (view, deactF) <- renderVal2 True nestedVal evs evAs
              let toggleCmd = case tc of
                      Open -> \k v -> Map.insert k v
                      Close -> \k v -> Map.delete k
                      Toggle -> \k v -> mapToggle k v
              sendTo subCompsU (toggleCmd k (view, deactF))

  let canMoveIntoItem :: Int -> Bool = \n -> case Map.lookup (safeIndex (Map.keys theValueMap) n) theValueMap of
            Just (Val _) -> True
            _            -> False

  subscribeEvent acc $ Sink $ \e -> do
    sendTo selU $ const $ Local 0
  subscribeEvent subCompToldUs $ Sink $ \e -> do
    s <- pollBehavior $ current selS
    case s of
      Nested n -> sendTo selU (const $ Local n)
      _ -> return ()
  subscribeEvent ev $ Sink $ \e -> case e of
    MoveUp  -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> sendTo selU (const $ Local (n - 1))
    MoveDown -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> sendTo selU (const $ Local (n + 1))
    MoveLeft -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n ->
          if not allowLeftMove
          then
            return ()
          else do
            sendTo selU (const $ Outside)
            sendTo tellSuperComp ()
    MoveRight -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n ->
          if not $ canMoveIntoItem n
          then
              return ()
          else do
            doWithSelected Open n
            sendTo selU (const $ Nested n)
            sendTo tellSubCompN n
    ToggleSelected -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> doWithSelected Toggle n

  -- --------------------------------------------------------------------------------
  -- RENDERING

  let view :: Signal Screen = do
        r :: Map String (Signal Screen) <- fmap (\m -> Map.map fst m) $ subCompsS
        s :: Sel <- selS
        curKey' :: Maybe String <- curLocalKey
        let footer :: Signal Screen = pure emptyScreen -- pure [show s]
        fmap catScreensV $ sequenceA $ Map.foldrWithKey (renderKeyWithValue curKey') [footer] $ useKeys theValueMap r
  pure (view, superCompTold)

  where

    renderKeyWithValue :: Maybe String -> String -> Maybe (Signal Screen) -> [Signal Screen] -> [Signal Screen]
    renderKeyWithValue curKey k Nothing  b = pure (renderKey curKey k) : fmap (moveScreenRight 2) (pure $ screenFromList ["..."]) : b
    renderKeyWithValue curKey k (Just v) b = pure (renderKey curKey k) : fmap (moveScreenRight 2) v : b

    renderKey :: Maybe String -> String -> Screen
    renderKey curKey k = screenFromList [k ++ if curKey == Just k then "*" else ""]

    -- Takes all the keys in first map and replaces with Just for keys that exist in the second map, Nothing for keys that doesnt
    useKeys :: Ord k => Map k b -> Map k a -> Map k (Maybe a)
    useKeys m1 m2 = Map.unionWith (\x y -> y) (fmap (const Nothing) m1) (fmap Just m2)

    mapToggle k v m = Map.alter g k m where
      g Nothing  = Just v
      g (Just _) = Nothing
    safeIndex xs n = xs !! (n `mod` length xs)


-- Like Show, drawing to a screen
class ToScreen a where
  toScreen :: a -> Screen


data ArrowKey = Up | Down | Left | Right
data Key      = ArrowKey ArrowKey | LetterKey Char


{-

A screen in a MxN matrix of characters
With a local origin (TODO), which is conceptually at the BL corner of one char

I.e. if in these 2 sreens, the locl origin is a the BL corner of the upper-case chr

  let a =
    xX
    xx
  let b =
    abC
  then, atop b a ==>
    abC
     xx

TODO wrap Scren type + add basic local origin/enveloped composition operators (beside, above, etc)
-}
newtype Screen   = Screen_ [[Char]] -- row-major order

emptyScreen = Screen_ mempty

{-
Make a screen from a list of rows.
Dimensions are given by the maximum number of rows and columns, and the local origin is at TL corner.
-}
screenFromList :: [String] -> Screen
screenFromList = Screen_

{-
Put screens on top of one another (top-to-bottom).
Preserves local origin of first screen.
-}
catScreensV :: [Screen] -> Screen
catScreensV xs = Screen_ $ mconcat $ fmap getScreen xs
  where
    getScreen (Screen_ x) = x

moveScreenRight :: Int -> Screen -> Screen
moveScreenRight n (Screen_ sc) = Screen_ $ fmap (replicate n ' ' ++) $ sc

-- above :: Screen -> Screen -> Screen
-- above = (<>)


{-
empty :: Screen
-- overlay screens, top-most character is rendered
atop :: Screen -> Screen -> Screen
-- overlay, composing characters as follows (if size differs, ' ' is used in its place)
atopWith :: (Char -> Char -> Char) -> Screen -> Screen -> Screen


transl :: V2 Int -> Screen -> Screen
-- rotate QT counter-clockwise
rotateQT :: Screen -> Screen
flipH :: Screen -> Screen
flipV :: Screen -> Screen
{-
Get number of characters from local origin to ((left,bottom),(right,top))
I.e. for these screenLines (agin, local origin at BL corner of upper-case)
  xX
    -> (1,0,1,1)

  X -> (0,0,1,1)

  Xx
  xx -> (0,-1,2,1)

-}
getBoundingBox :: Screen -> (V2 Int,V2 Int)
-- like digrams, i.e. move second element
juxtapose :: (Left|Right|Up|Down) -> Screen -> Screen -> Screen
place :: (Left|Right|Up|Down) -> Screen -> Screen -> Screen



-}


main :: IO ()
main = do
  hideCursor

  clearScreen
  setCursorPosition 0 0
  putStrLn "Welcome! (ctrl-c to exit)"
  (cU, cE) <- newEvent
  app <- explorerApp cE
  void $ subscribeEvent (updates app) $ Sink $ \screen -> do

    -- TODO get terminal size through https://hackage.haskell.org/package/terminal-size
    -- Or go full ncurses (what's the difference in portability?)
    let lines = renderScreen (80, 60) screen
    setCursorPosition 0 0
    forM_ lines putStrLn
  hSetEcho stdout False

  hSetBuffering stdin NoBuffering
  forever $ do
    -- TODO filter escape sequences
    -- At least "ESC[A" etc for arrow keys

    -- TODO move char input to separate thread (?) so this loop can
    -- poll for new keys and process other inputs too (i.e. time)
    c <- getChar
    sendTo cU $ LetterKey c

  where
    renderScreen
      :: (Int, Int) -- X,Y dimensions of viewport (i.e number of columns/rows)
      -> Screen -- incoming
      -> [[Char]]
    renderScreen (width, height) (Screen_ xs) = padWithTo (replicate width '+') height $ fmap (padWithTo ' ' width) xs
      where
        padWithTo fillerElem n xs = take n $ xs ++ repeat fillerElem


newSignal :: a -> FRP (Sink a, Signal a)
newSignal z = do
  (u, e) <- newEvent
  s <- stepperS z e
  pure (u, s)

newSignalA :: a -> FRP (Sink (a -> a), Signal a)
newSignalA z = do
  (u, e) <- newEvent
  s <- accumS z e
  pure (u, s)
