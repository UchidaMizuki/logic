{-# LANGUAGE ViewPatterns #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Fix (Fix (..))

someFunc :: IO ()
someFunc = do
  -- state <- bottom
  -- control <- bottom
  -- false state control
  -- not_ state control -- not False == True

  state <- bottom
  control <- bottom
  forkIO $ false state control
  forkIO $ false state control
  not_ state control

  print "OK!"

bottom :: IO (Fix MVar)
bottom = Fix <$> newEmptyMVar

true :: Fix MVar -> Fix MVar -> IO ()
true = putMVar . unFix

false :: Fix MVar -> Fix MVar -> IO ()
false = const ((bottom >>=) . true)
-- false _ x = bottom >>= true x 

observe :: Fix MVar -> IO ()
observe = liftA2 (>>=) takeMVar putMVar . unFix
-- observe (unFix -> x) = takeMVar x >>= putMVar x

not_ :: Fix MVar -> Fix MVar -> IO ()
not_ x y = do 
  forkIO $ observe x
  observe y
  true x y