
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lubeck.FRP
import System.IO
import System.Console.ANSI
import BasePrelude hiding (Signal)
-- import Lib
import Linear.Affine
import Linear.V2




type Ship = Signal (String, V2 Int) -- name, pos





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
type Screen   = [[Char]] -- row-major order

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
