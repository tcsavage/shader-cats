module TestShaders where

import ShaderNode
import Types

import Control.Category
import Linear

import Prelude hiding (id, (.), fst, snd, curry, uncurry, Functor(..))

shaderFn (Diffuse norm f) = f
shaderFn (Reflection n eta f) = f
shaderFn (Mix fac a b f) = f

runTest shader = shaderFn shader $ V3 0.5 0.5 0.5

diffuse :: Color -> Node a (Closure Color)
diffuse color = Fmap (partial (Prim MulP) (NumL color)) . Prim DiffuseP . Const NormalP

testReflection :: Node a (Closure Color)
testReflection = Fmap (partial (Prim AddP) (NumL (V3 0.2 0 0)))
               . partial (Prim MixP) (NumL 0.5)
               . Fork (partial (Prim ReflectionP) (NumL 1.0)) (Prim DiffuseP)
               . Const NormalP

{-
testReflection :: ShaderA
testReflection = proc void -> do
    n <- normal -< void
    c <- reflection -< (n, 1.0)
    d <- diffuse -< n
    m <- mix -< (0.5, c, d)
    reflected -< ((V3 0.2 0 0) +) <$> m
-}
