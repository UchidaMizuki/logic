{-# LANGUAGE ViewPatterns #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Fix (Fix (..))
-- import Prelude hiding (not)

someFunc :: IO ()
someFunc = do
  clFalse >>= clNot >>= clObserve

  print "OK!"

data ConcurrentLogic = ConcurrentLogic
  { cl0 :: Fix MVar,
    cl1 :: Fix MVar,
    cl2 :: Fix MVar
  }

clObserve :: ConcurrentLogic -> IO ()
clObserve (ConcurrentLogic cl0 _ _) = takeMVar (unFix cl0) >>= putMVar (unFix cl0)

clBottom :: IO (Fix MVar)
clBottom = Fix <$> newEmptyMVar

clTrue :: IO ConcurrentLogic
clTrue = do
  cl0 <- clBottom
  cl1 <- clBottom
  cl2 <- clBottom
  putMVar (unFix cl1) cl0
  return $ ConcurrentLogic cl0 cl1 cl2

clFalse :: IO ConcurrentLogic
clFalse = do
  cl1 <- clBottom
  cl2 <- clBottom
  cl3 <- clBottom
  putMVar (unFix cl1) cl3
  return $ ConcurrentLogic cl1 cl2 cl3

clNot :: ConcurrentLogic -> IO ConcurrentLogic
clNot cl@(ConcurrentLogic cl1 cl2 cl3) = do
  forkIO $ clObserve cl
  
  cl0 <- clBottom
  let cl = ConcurrentLogic cl0 cl1 cl2
  forkIO $ clObserve cl

  takeMVar (unFix cl2) >>= putMVar (unFix cl3)
  return cl