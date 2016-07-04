
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

main' :: Events Key -> FRP (Signal Screen)
main' e = do
  renderVal exValue (filterJust $ fmap g e)
  where
    g (LetterKey '\r') = Just ToggleSelected
    g (LetterKey ' ') = Just ToggleSelected
    g (LetterKey 'w') = Just MoveUp
    g (LetterKey 's') = Just MoveDown
    g (LetterKey 'd') = Just MoveRight
    g (LetterKey 'a') = Just MoveLeft
    g _ = Nothing

renderVal
    :: Value
    -> Events Action
    -> FRP (Signal Screen)
renderVal x e = do
  (u,e2) <- newEvent
  r <- renderVal2 x e e2
  sendTo u ()
  pure (fst r)



-- TODO prevent going where you're not supposed to (i.e. messing up selection state)
-- TODO coloured output for selection
-- TODO only render innermost selection (so use a version of curKey that looks at Local only, not Nested)
-- TODO wrap Scren type + add basic local origin/enveloped composition operators (beside, above, etc)
-- TODO function to render a screen to a [[Char]] matrix of a specified with, padding with whitespace

renderVal2
    :: Value
    -> Events Action
    -> Events () -- accept selection
    -> FRP (Signal Screen, Future ())
renderVal2 (Str x) ev _   = pure $ (pure [x], fmap (const ()) $ Lubeck.FRP.filter (== MoveRight) ev)
renderVal2 (Val theValueMap) ev acc = do

  -- TODO partial iso between strings and keys here

  -- State:
  --  selS      :: S ? determining current selection: in this tree, in the given subtree, or outside this element
  --  toggledS  :: (Set String) which elements are toggled
  (selU, selS :: Signal Sel)                  <- newSignalA Outside
  let curKey :: Signal (Maybe String) = flip fmap selS  $ \s -> case s of
        Local n -> Just $ safeIndex (Map.keys theValueMap) n
        Nested n -> Just $ safeIndex (Map.keys theValueMap) n
        _ -> Nothing


  -- NOTE this would be redunant if we actually removed subcomponents from map below
  -- (toggledU, toggledS :: Signal (Set String)) <- newSignalA mempty

  -- TODO subcomponents, created on demand
  (subCompsU, subCompsS :: Signal (Map String (Signal Screen, Future ()))) <- newSignalA mempty



  let subCompToldUs :: Events Int = joinE $ updates $ fmap (mconcat . zipWith (\n fut -> fmap (const n) fut) [0..] . fmap snd . toList) subCompsS

  (currentSubCompS :: Sink Action, currentSubCompE) <- newEvent -- undefined
  (tellSubCompN  :: Sink Int, tellSubCompE) <- newEvent -- undefined

  let foo1 :: String -> Events Action = \n -> filterJust $ snapshotWith (\k v -> if k == Just n then Just v else Nothing) (current curKey) currentSubCompE
  let foo2 :: String -> Events ()     = \k -> filterJust $ fmap (\n -> if safeIndex (Map.keys theValueMap) n == k then Just () else Nothing ) tellSubCompE

  (tellSuperComp :: Sink (), superCompTold :: Future ()) <- newEvent

  subscribeEvent acc $ Sink $ \e -> do
    sendTo selU $ const $ Local 0
  subscribeEvent subCompToldUs $ Sink $ \e -> do
    s <- pollBehavior $ current selS
    case s of
      Nested n -> sendTo selU (const $ Local n)
      _ -> return ()
  subscribeEvent ev $ Sink $ \e -> case e of
    MoveUp -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> sendTo selU (const $ Local (n-1))
    MoveDown -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> sendTo selU (const $ Local (n+1))
    MoveLeft -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> do
          sendTo selU (const $ Outside)
          sendTo tellSuperComp ()
    MoveRight -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> do
          sendTo selU (const $ Nested n)
          sendTo tellSubCompN n
    ToggleSelected -> do
      s <- pollBehavior $ current selS
      case s of
        Outside -> return ()
        Nested n -> sendTo currentSubCompS e
        Local n -> do
          let k = safeIndex (Map.keys theValueMap) n
          case Map.lookup k theValueMap of
            Nothing -> error "Impossible"
            Just nestedVal -> do
              let evs :: Events Action = foo1 k
              let evAs :: Events () = foo2 k
              -- subscribeEvent evs $ Sink $ \x -> putStr "evs: " >> putStr k >> putStr " " >> print x
              -- subscribeEvent ev  $ Sink $ \x -> putStr "ev : " >> putStr k >> putStr " " >> print x
              (view, deactF) <- renderVal2 nestedVal evs evAs
              sendTo subCompsU (mapToggle k (view, deactF))

  -- TODO we also need to render our own keys and selection...
  let foo2 :: Signal Screen = do
        r :: Map String (Signal Screen) <- fmap (\m -> Map.map fst m) $ subCompsS
        s :: Sel <- selS
        curKey :: Maybe String <- curKey
        let footer :: Signal Screen = mempty -- pure [show s]
        mconcat $ Map.foldrWithKey (renderKeyWithValue curKey) [footer] $ useKeys theValueMap r -- TODO use useKeys
  pure (foo2, superCompTold)
  where
    renderKeyWithValue :: Maybe String -> String -> Maybe (Signal Screen) -> [Signal Screen] -> [Signal Screen]
    renderKeyWithValue curKey k Nothing  b = pure (renderKey curKey k) : fmap (moveScreenRight 2) (pure ["..."]) : b
    renderKeyWithValue curKey k (Just v) b = pure (renderKey curKey k) : fmap (moveScreenRight 2) v : b

    renderKey :: Maybe String -> String -> Screen
    renderKey curKey k = [k ++ if curKey == Just k then "*" else ""]

    -- Takes all the keys in first map and replaces with Just for keys that exist in the second map, Nothing for keys that doesnt
    useKeys :: Ord k => Map k b -> Map k a -> Map k (Maybe a)
    useKeys m1 m2 = Map.unionWith (\x y -> y) (fmap (const Nothing) m1) (fmap Just m2)

    mapToggle k v m = Map.alter g k m where
      g Nothing  = Just v
      g (Just _) = Nothing
    safeIndex xs n = cycle xs !! abs n

-- renderVal :: Value -> Events Action -> FRP (Signal Screen)
-- renderVal (Str x) _  = pure $ pure $ [x]
-- renderVal (Val m) ev = do
--   let selectedS :: Signal (Maybe String) = undefined
--   let openedS :: Signal (Set String) = undefined
--   -- subscribeEvent ev $ Sink $ \e -> case e of
--   --   MoveUp -> return ()
--   --     -- if something is selected, that thing is not opened and we can move up
--   --   MoveDown -> return ()
--   --   Collapse -> return ()
--   --   Decollapse -> return ()
--   (r :: Signal Screen) <- fmap apScreensM $ forM (Map.toList m) $ \(k,v) -> do
--     if Set.member k (undefined :: Set String) -- TODO
--       then
--         renderVal v ev
--       else
--         return $ collapsed
--
--   return undefined
--   where
--     apScreensM :: Applicative f => [f Screen] -> f Screen
--     apScreensM = fmap apScreens . sequenceA
--     apScreens :: [Screen] -> Screen
--     apScreens = foldr above mempty
--     collapsed :: Signal Screen
--     collapsed = pure ["..."]



-- subWidget l w = dimap w (set l) (view l)

-------TODO move
-- -- | Map over the HTML rendering of a widget.
-- mapHtmlWidget :: (r -> r2) -> WidgetT r a b -> WidgetT r2 a b
-- mapHtmlWidget f w = \s -> f . w s
-- -- TODO renamve mapView or similar
--
-- -- | Turn a widget of a smaller type into a widget of a larger type using a lens.
-- --
-- -- The resulting widget will render part of the larger type that the lens selects.
-- -- It the resulting widget produces output, this will be used to transform the
-- -- value rendered by the larger widget. This makes this function especially
-- -- useful in conjunction with 'multiWidget'.
-- --
-- -- @
-- -- subWidget _1     :: Widget' a -> Widget' (a, b)
-- -- subWidget (at 1) :: Widget' (Just a) -> Widget' (Map Int a)
-- -- @
-- subWidget :: Lens' s a -> Widget' a -> Widget' s
-- subWidget l w o i = w (contramapSink (\x -> set l x i) o) (view l i)
--
-- -- | Compose two widgets.
-- -- Both render the value and the resultant HTML is composed using the given function.
-- -- Output emitted by either widget is emitted by the resultant widget.
-- bothWidget :: (Html -> Html -> Html) -> Widget a b -> Widget a b -> Widget a b
-- bothWidget c w1 w2 o i = w1 o i `c` w2 o i
--
-- -- | Compose a widget out of several widgets of the same type.
-- -- Useful in conjunction with 'subWidget', as in
-- --
-- -- @
-- -- data Many = Many { _foo :: Int, _bar :: Maybe String }
-- -- makeLenses ''Many
-- --
-- -- manyW :: Widget Many Many
-- -- manyW = multiWidget (\x y -> div () [x,y])
-- --   [subWidget foo intW, subWidget bar (maybeW (div () ()) stringW)]
-- -- @
-- multiWidget :: Foldable t => (Html -> Html -> Html) -> t (Widget a b) -> Widget a b
-- multiWidget f = foldr1 (bothWidget f)
--
--
-- isoW :: Iso' a b -> Widget' a -> Widget' b
-- isoW i = dimapWidget (review i) (view i)
--
-- -- | Turn a widget of a part type into a widget of an alternative type using a prism.
-- --
-- -- The resulting widget will render the part of the value selected by the prism if it
-- -- is there, otherwise it renders the given default HTML. The default rendering can
-- -- optionally use the provided sink to initiate a new value (typically some kind of
-- -- default), in which case this value immediately replaces the previous value.
-- --
-- -- @
-- -- possW _Just :: Widget' a -> Widget' (Maybe a)
-- -- possW _Left :: Widget' a -> Widget' (Either a b)
-- -- @
-- possW :: (Sink a -> Html) -> Prism' s a -> Widget' a -> Widget' s
-- possW z p w o i = case preview p i of
--   Nothing -> z (contramapSink (review p) o)
--   Just x  -> w (contramapSink (review p) o) x
--
-- maybeW :: Html -> Widget' a -> Widget' (Maybe a)
-- maybeW z = possW (const z) Control.Lens._Just
--
-- mapMWidget :: ([Html] -> Html) -> Widget a a -> Widget [a] a
-- mapMWidget k w o is = k $ fmap (w o) is

-------







-- type Ship = Signal (String, V2 Int) -- name, pos




{-




-}

-- Like Show, drawing to a screen
class ToScreen a where
  toScreen :: a -> Screen



{-
Note: can react to input but not anything else.
We should provide a signal with time, and other stuff.
-}
-- main' :: Events Key -> FRP (Signal Screen)
-- main' e = do
--   stepperS ["abc", "def"] $ fmap (\(LetterKey c) -> (take 55 $ cycle $ [take 80 $ cycle "abc", "de"++[c]])) e





--
-- TODO library:

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
-}
type Screen   = [[Char]] -- row-major order

moveScreenRight :: Int -> Screen -> Screen
moveScreenRight n sc = fmap (replicate n ' ' ++) sc

above :: Screen -> Screen -> Screen
above = (<>)


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
  app <- main' cE
  void $ subscribeEvent (updates app) $ Sink $ \screenLines -> do
    -- putStrLn "Hello!"
    -- clearScreen
    setCursorPosition 0 0
    -- TODO pad lines to max lenght
    forM_ screenLines (putStrLn . padTo 80)
    putStrLn "-----------" -- TODO pad bottom properly
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
    padTo n xs = take n $ xs ++ repeat ' '

newSignal :: a -> FRP (Sink a, Signal a)
newSignal z = do
  (u, e) <- newEvent
  s <- stepperS z e
  pure (u, s)

-- TODO move to Lubeck
newSignalA :: a -> FRP (Sink (a -> a), Signal a)
newSignalA z = do
  (u, e) <- newEvent
  s <- accumS z e
  pure (u, s)
