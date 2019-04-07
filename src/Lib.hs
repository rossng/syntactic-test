{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
    MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell,
    TypeFamilies, TypeOperators, UndecidableInstances #-}

module Lib where

import Data.Typeable

import Language.Syntactic
import qualified Language.Syntactic as Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Interpretation as Interpretation
import Language.Syntactic.TH

class (Typeable a, Show a, Eq a, Ord a) => Type a
instance (Typeable a, Show a, Eq a, Ord a) => Type a

-- Arithmetic

data ExpArith sig
    where
        ExpInt :: Int -> ExpArith (Full Int)
        ExpAdd :: (Type a, Num a) => ExpArith (a :-> a :-> Full a)
        ExpSub :: (Type a, Num a) => ExpArith (a :-> a :-> Full a)
        ExpMul :: (Type a, Num a) => ExpArith (a :-> a :-> Full a)

deriveSymbol ''ExpArith
deriveEquality ''ExpArith

instance Render ExpArith
    where
        renderSym (ExpInt a) = show a
        renderSym ExpAdd = "(+)"
        renderSym ExpSub = "(-)"
        renderSym ExpMul = "(*)"
        renderArgs = renderArgsSmart

instance Eval ExpArith
    where
        evalSym (ExpInt a) = a
        evalSym ExpAdd = (+)
        evalSym ExpSub = (-)
        evalSym ExpMul = (*)

-- Booleans

data ExpBool sig
    where
        ExpAnd :: ExpBool (Bool :-> Bool :-> Full Bool)
        ExpOr :: ExpBool (Bool :-> Bool :-> Full Bool)

deriveSymbol ''ExpBool
deriveEquality ''ExpBool

instance Render ExpBool
    where
        renderSym ExpAnd = "(&&)"
        renderSym ExpOr = "(||)"
        renderArgs = renderArgsSmart

instance Eval ExpBool
    where
        evalSym ExpAnd = (&&)
        evalSym ExpOr = (||)

-- Comparisons

data ExpComp sig
    where
        ExpLt :: (Num a, Ord a) => ExpComp (a :-> a :-> Full Bool)
        ExpGt :: (Num a, Ord a) => ExpComp (a :-> a :-> Full Bool)

deriveSymbol ''ExpComp
deriveEquality ''ExpComp

instance Render ExpComp
    where
        renderSym ExpLt = "(<)"
        renderSym ExpGt = "(>)"
        renderArgs = renderArgsSmart

instance Eval ExpComp
    where
        evalSym ExpLt = (<)
        evalSym ExpGt = (>)

-- AST

type ExpDomain = ExpArith :+: ExpBool :+: ExpComp
type Expression a = ASTF ExpDomain a

expInt a = Sym (inj $ ExpInt a)
expAdd a b = Sym (inj ExpAdd) :$ a :$ b
expSub a b = Sym (inj ExpSub) :$ a :$ b
expMul a b = Sym (inj ExpMul) :$ a :$ b

expAnd a b = Sym (inj ExpAnd) :$ a :$ b
expOr a b = Sym (inj ExpOr) :$ a :$ b

expLt a b = Sym (inj ExpLt) :$ a :$ b
expGt a b = Sym (inj ExpGt) :$ a :$ b

-- Examples

simpleExpression :: Expression Int
simpleExpression = expInt 5

complexExpression :: Expression Bool
complexExpression = expLt twenty thirty
    where
        twenty = expMul (expInt 5) (expInt 4)
        thirty = expMul (expInt 10) (expInt 3)

prettyPrint :: Expression a -> String
prettyPrint = render