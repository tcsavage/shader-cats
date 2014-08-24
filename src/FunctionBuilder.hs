{-# LANGUAGE GADTs #-}

module FunctionBuilder where

import ShaderNode
import Types

import qualified Control.Categorical.Functor as CF
import Control.Category
import Control.Applicative hiding (Const(..))
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State

import Prelude hiding (id, (.))

-- This is wrong.
curryState :: ((a, b) -> State s c) -> a -> State s (b -> c)
curryState f a = state (\s -> (\b -> evalState (curry f a b) s, s))

uncurryState :: (a -> State s (b -> c)) -> (a, b) -> State s c
uncurryState f (a, b) = fmap (\f' -> f' b) (f a)

toFn :: Node a b -> ShaderGlobals -> a -> State (Closure Color) b
toFn Id = const (pure . id)
toFn (Comp bc ab) = \glob -> (toFn bc) glob <=< (toFn ab) glob
toFn Exl = const (pure . fst)
toFn Exr = const (pure . snd)
toFn (Fork ab ac) = \glob -> runKleisli $ (Kleisli $ toFn ab glob) &&& (Kleisli $ toFn ac glob)
toFn Inl = const (pure . Left)
toFn Inr = const (pure . Right)
toFn (Join ba ca) = \glob -> runKleisli $ (Kleisli $ toFn ba glob) ||| (Kleisli $ toFn ca glob)
toFn Apply = const $ \(f, x) -> pure $ f x
toFn (Curry node) = \glob -> curryState $ toFn node glob  -- Dodgy
toFn (Uncurry node) = \glob -> uncurryState $ toFn node glob
toFn (Prim prim) = primFn prim
toFn (Const prim) = \glob _ -> primConst prim glob
toFn (Fmap node) = \glob -> pure . CF.fmap (toFnPure node)  -- Also dodgy

primFn :: Prim (a -> b) -> ShaderGlobals -> a -> State (Closure Color) b
primFn AddP = \glob a -> pure $ (+) a
primFn MulP = \glob a -> pure $ (*) a
primFn DiffuseP = \glob norm -> pure $ Diffuse norm id

primConst :: Prim a -> ShaderGlobals -> State (Closure Color) a
primConst (LitP lit) = const $ pure $ primLit lit
primConst NormalP = \glob -> pure $ gNormal glob
