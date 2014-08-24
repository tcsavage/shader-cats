module TestShaders where

import ShaderNode
import Types

import Control.Category
import Linear

import Prelude hiding (id, (.), fst, snd, curry, uncurry, Functor(..))

diffuseFn (Diffuse norm f) = f

runDiff diff = diffuseFn diff $ V3 0.5 0.5 0.5

diffuse :: Color -> Node a (Closure Color)
diffuse color = Fmap (partial (Prim MulP) (NumL color)) . Prim DiffuseP . Const NormalP
