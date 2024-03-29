{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, RankNTypes, FlexibleContexts, FlexibleInstances #-}

module ShaderNode where

import Types

import Control.Categorical.Bifunctor
import Control.Categorical.Functor
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Category.Monoidal
import Data.Void
import Linear
import Text.Printf
import Prelude hiding (id, (.), fst, snd, curry, uncurry, Functor(..))
import qualified Prelude (Functor(..))

-- | Core node language.
data Node :: * -> * -> * where
    -- Category
    Id      :: Node a a
    Comp    :: Node b c -> Node a b -> Node a c
    -- Products
    Exl     :: Node (a, b) a
    Exr     :: Node (a, b) b
    Fork    :: Node a b -> Node a c -> Node a (b, c)
    -- Coproducts
    Inl     :: Node a (Either a b)
    Inr     :: Node b (Either a b)
    Join    :: Node b a -> Node c a -> Node (Either b c) a
    -- Exponentials
    Apply   :: Node ((a -> b), a) b
    Curry   :: Node (a, b) c -> Node a (b -> c)
    Uncurry :: Node a (b -> c) -> Node (a, b) c
    -- Primitives
    Prim    :: Prim (a -> b) -> Node a b
    Const   :: Prim b -> Node a b
    -- Functor
    Fmap    :: forall a b f. Functor f (->) (->) => Node a b -> Node (f a) (f b)

type Input b = Node Void b
type SurfaceShader = Node Void (Closure Color)

-- | Literals.
data Lit :: * -> * where
    VoidL :: forall a. Lit a
    UnitL :: Lit ()
    NumL :: forall a. (Num a, Show a) => a -> Lit a  -- Also includes vector types (as they are also Nums)

-- | Primitive functions.
data Prim :: * -> * where
    LitP :: Lit a -> Prim a
    AddP :: forall a. Num a => Prim (a -> a -> a)
    MulP :: forall a. Num a => Prim (a -> a -> a)
    NormalP :: Prim (V3 Scalar)
    DiffuseP :: Prim (V3 Scalar -> Closure Color)
    ReflectionP :: Prim (Scalar -> V3 Scalar -> Closure Color)
    MixP :: Prim (Scalar -> (Closure Color, Closure Color) -> Closure Color)

-- | Shader closures.
data Closure a = None
               | Shadeless a
               | Mix Scalar (Closure Color) (Closure Color) (Color -> a)
               | Emissive a
               | Diffuse (V3 Scalar) (Color -> a)
               | Reflection (V3 Scalar) Scalar (Color -> a)

-- | Build a function from nodes.
toFn :: Node a b -> ShaderGlobals -> a -> b
toFn Id = const id
toFn (Comp bc ab) = \glob -> toFn bc glob . toFn ab glob
toFn Exl = const fst
toFn Exr = const snd
toFn (Fork ab ac) = \glob -> toFn ab glob &&& toFn ac glob
toFn Inl = const Left
toFn Inr = const Right
toFn (Join ba ca) = \glob -> toFn ba glob ||| toFn ca glob
toFn Apply = const $ \(f, x) -> f x
toFn (Curry node) = \glob -> curry $ toFn node glob
toFn (Uncurry node) = \glob -> uncurry $ toFn node glob
toFn (Prim prim) = primFn prim
toFn (Const prim) = \glob _ -> primConst prim glob
toFn (Fmap node) = \glob -> fmap (toFn node glob)

primFn :: Prim (a -> b) -> ShaderGlobals -> a -> b
primFn AddP = const (+)
primFn MulP = const (*)
primFn DiffuseP = \glob norm -> Diffuse norm id
primFn ReflectionP = \glob eta norm -> Reflection norm eta id
primFn MixP = \glob fac (a, b) -> Mix fac a b id

primConst :: Prim a -> ShaderGlobals -> a
primConst (LitP lit) = const $ primLit lit
primConst NormalP = \glob -> gNormal glob

primLit :: Lit a -> a
primLit VoidL = error "Void!"
primLit UnitL = ()
primLit (NumL x) = x

partial :: Node a (b -> c) -> Lit a -> Node b c
partial node x = Uncurry node . Fork (Const $ LitP x) Id

-----------------
-- Node instances
-----------------

instance Show (Node a b) where
    show Id = "id"
    show (Comp l r) = printf "(%s . %s)" (show l) (show r)
    show Exl = "fst"
    show Exr = "snd"
    show (Fork l r) = printf "(%s &&& %s)" (show l) (show r)
    show Inl = "Left"
    show Inr = "Right"
    show (Join l r) = printf "(%s ||| %s)" (show l) (show r)
    show Apply = "apply"
    show (Curry x) = printf "(curry %s)" (show x)
    show (Uncurry x) = printf "(uncurry %s)" (show x)
    show (Prim x) = printf "(prim %s)" (show x)
    show (Const x) = printf "(const %s)" (show x)
    show (Fmap x) = printf "(fmap %s)" (show x)

instance Category Node where
    id = Id
    (.) = Comp

instance PFunctor (,) Node Node where
    first f = bimap f id

instance QFunctor (,) Node Node where
    second = bimap id

instance Bifunctor (,) Node Node Node where
    bimap = bimapProduct

instance PFunctor Either Node Node where
    first f = bimap f id

instance QFunctor Either Node Node where
    second = bimap id

instance Bifunctor Either Node Node Node where
    bimap = bimapSum

instance Associative Node (,) where
    associate = associateProduct
    disassociate = disassociateProduct

instance Associative Node Either where
    associate = associateSum
    disassociate = disassociateSum

instance Braided Node (,) where
    braid = braidProduct

instance Symmetric Node (,)

instance Braided Node Either where
    braid = braidSum

instance Symmetric Node Either

instance Monoidal Node (,) where
    type Id Node (,) = ()
    idl = Exr
    idr = Exl
    coidr = Fork Id (Const $ LitP UnitL)
    coidl = Fork (Const $ LitP UnitL) Id

instance Monoidal Node Either where
    type Id Node Either = Void
    idl = Join (Const $ LitP VoidL) Id
    idr = Join Id (Const $ LitP VoidL)
    coidl = Inr
    coidr = Inl

instance Cartesian Node where
    type Product Node = (,)
    fst = Exl
    snd = Exr
    diag = Fork Id Id
    (&&&) = Fork

instance CoCartesian Node where
    type Sum Node = Either
    inl = Inl
    inr = Inr
    codiag = Join Id Id
    (|||) = Join

instance CCC Node where
    type Exp Node = (->)
    apply = Apply
    curry = Curry
    uncurry = Uncurry

---------------------
-- Prim/Lit instances
---------------------

instance Show (Lit a) where
    show VoidL = "VOID"
    show UnitL = "()"
    show (NumL x) = show x

instance Show (Prim a) where
    show (LitP lit) = show lit
    show AddP = "(+)"
    show MulP = "(*)"
    show NormalP = "normal"
    show DiffuseP = "diffuse"
    show ReflectionP = "reflection"
    show MixP = "mix"

--------------------
-- Closure instances
--------------------

instance Show a => Show (Closure a) where
    show None = "None"
    show (Shadeless a) = printf "(Shadeless %s)" $ show a
    show (Emissive _) = "Emissive"
    show (Mix fact c0 c1 _) = printf "(Mix %f %s %s)" fact (show c0) (show c1)
    show (Diffuse norm _) = printf "Diffuse %s" (show norm)
    show (Reflection _ _ _) = "Reflection"

instance Functor Closure Node Node where
    fmap = Fmap

instance Functor Closure (->) (->) where
    fmap _ None = None
    fmap f (Shadeless a) = Shadeless $ f a
    fmap f (Emissive a) = Emissive $ f a
    fmap f (Mix fact a b c) = Mix fact a b (f . c)
    fmap f (Diffuse norm a) = Diffuse norm (f . a)
    fmap f (Reflection norm eta a) = Reflection norm eta (f . a)

-------------
-- V3 Functor
-------------

instance Functor V3 (->) (->) where
    fmap = Prelude.fmap
