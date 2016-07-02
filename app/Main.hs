
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lubeck.FRP
import System.IO
import System.Console.ANSI
import BasePrelude hiding (Signal)
-- import Lib
import Linear.Affine
import Linear.V2
-- import Lubeck.Forms (WidgetT(..))




type Ship = Signal (String, V2 Int) -- name, pos




{-




-}

-- Like Show, drawing to a screen
class ToScreen a where
  toScreen :: a -> Screen



{-
Note: can react to input but not anything else.
We should provide a signal with time, and other stuff.
-}
main' :: Events Key -> FRP (Signal Screen)
main' e = do
  stepperS ["abc", "def"] $ fmap (\(LetterKey c) -> (take 30 $ cycle $ ["abc", "de"++[c]])) e





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
  void $ subscribeEvent (updates app) $ \screenLines -> do
    -- putStrLn "Hello!"
    clearScreen
    setCursorPosition 0 0
    forM_ screenLines putStrLn
  hSetEcho stdout False

  hSetBuffering stdin NoBuffering
  forever $ do
    -- TODO filter escape sequences
    -- At least "ESC[A" etc for arrow keys

    -- TODO move char input to separate thread (?) so this loop can
    -- poll for new keys and process other inputs too (i.e. time)
    c <- getChar
    cU $ LetterKey c
