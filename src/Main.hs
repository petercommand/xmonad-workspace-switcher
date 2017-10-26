module Main where

import Control.Monad.Reader
import Graphics.UI.Gtk (initGUI)
import XMonad.Actions.WorkspaceSwitcher

main :: IO ()
main = do
  initGUI
  let config = SwitcherConfig { scale  = 15
                              , cols   = 3
                              , xstate = Nothing
                              }
  runReaderT switcherMain config
  return ()
