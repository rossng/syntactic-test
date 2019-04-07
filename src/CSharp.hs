{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
    MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell,
    TypeFamilies, TypeOperators, UndecidableInstances #-}

module CSharp where

import Data.Typeable
import Data.Bits
import Data.Word
import Data.Decimal
import Data.Int

import qualified Data.Hash as Hash

import Language.Syntactic
import qualified Language.Syntactic as Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Interpretation as Interpretation
import Language.Syntactic.TH

class (Typeable a, Show a, Eq a, Ord a) => Type a
instance (Typeable a, Show a, Eq a, Ord a) => Type a

type CSBool = Bool
type CSByte = Word8
type CSSByte = Word8
type CSChar = Char
type CSDecimal = Decimal
type CSDouble = Double
type CSSingle = Float
type CSInt = Int32
type CSUInt = Word32
type CSLong = Int64
type CSULong = Word64
type CSShort = Int16
type CSUShort = Word16
type CSString = String

class Shiftable a
instance Shiftable CSInt
instance Shiftable CSUInt
instance Shiftable CSLong
instance Shiftable CSULong

class Incrementable a
instance Incrementable CSSByte
-- TODO: CSByte is a duplicate definition. Maybe use newtypes?
instance Incrementable CSChar
instance Incrementable CSDecimal
instance Incrementable CSDouble
instance Incrementable CSSingle
instance Incrementable CSInt
instance Incrementable CSUInt
instance Incrementable CSLong
instance Incrementable CSULong
instance Incrementable CSShort
instance Incrementable CSUShort

data TType = TType [Int] String
    deriving (Eq)

instance Hash.Hashable TType
    where hash (TType ds n) = Hash.hash ds `Hash.combine` Hash.hash n

data Exp a
data UnExp a
data PrimExp a

-- Expression

data Expression sig
    where
        ExpTern :: Expression (Exp CSBool :-> Exp a :-> Exp a :-> Full (Exp a))
        ExpOr :: Expression (Exp CSBool :-> Exp CSBool :-> Full (Exp CSBool))
        ExpAnd :: Expression (Exp CSBool :-> Exp CSBool :-> Full (Exp CSBool))
        ExpBitOr :: Bits a => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpBitXor :: Bits a => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpBitAnd :: Bits a => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpEq :: Eq a => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpNeq :: Eq a => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpLt :: (Num a, Ord a) => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpGt :: (Num a, Ord a) => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpLte :: (Num a, Ord a) => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpGte :: (Num a, Ord a) => Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpIsE :: Expression (Exp a :-> Exp a :-> Full (Exp CSBool))
        ExpIsT :: TType -> Expression (Exp a :-> Full (Exp CSBool))
        ExpAs :: TType -> Expression (Exp a :-> Full (Exp CSBool))
        ExpLs :: (Shiftable a) => Expression (Exp a :-> Exp CSInt :-> Full (Exp a))
        ExpRs :: (Shiftable a) => Expression (Exp a :-> Exp CSInt :-> Full (Exp a))
        ExpAdd :: (Num a) => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpSub :: (Num a) => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpMul :: (Num a) => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpDiv :: (Num a) => Expression (Exp a :-> Exp a :-> Full (Exp a))
        ExpMod :: (Num a) => Expression (Exp a :-> Exp a :-> Full (Exp a))

deriveSymbol ''Expression
deriveEquality ''Expression

data IncrementPosition = Pre | Post
data Pointer = Pointer

data UnaryExpression sig
    where
        ExpUnPos :: Num a => UnaryExpression (UnExp a :-> Full (UnExp a))
        ExpUnNeg :: Num a => UnaryExpression (UnExp a :-> Full (UnExp a))
        ExpUnInc :: Incrementable a => IncrementPosition -> UnaryExpression (UnExp a :-> Full (UnExp a))
        ExpUnDec :: Incrementable a => IncrementPosition -> UnaryExpression (UnExp a :-> Full (UnExp a))
        ExpUnBoolNot :: UnaryExpression (UnExp CSBool :-> Full (UnExp CSBool))
        ExpUnBitNot :: Bits a => UnaryExpression (UnExp a :-> Full (UnExp a))
        ExpUnDeref :: UnaryExpression (UnExp Pointer :-> Full (UnExp a))
        ExpUnCast :: TType -> UnaryExpression (UnExp a :-> Full (UnExp b))
        ExpUnPrim :: UnaryExpression (PrimExp a :-> Full (PrimExp a))

-- instance Render ExpArith
--     where
--         renderSym (ExpInt a) = show a
--         renderSym ExpAdd = "(+)"
--         renderSym ExpSub = "(-)"
--         renderSym ExpMul = "(*)"
--         renderArgs = renderArgsSmart

-- instance Eval ExpArith
--     where
--         evalSym (ExpInt a) = a
--         evalSym ExpAdd = (+)
--         evalSym ExpSub = (-)
--         evalSym ExpMul = (*)

-- -- Unary Expression

-- -- Primary Expression



-- -- Booleans

-- data ExpBool sig
--     where
--         ExpAnd :: ExpBool (Bool :-> Bool :-> Full Bool)
--         ExpOr :: ExpBool (Bool :-> Bool :-> Full Bool)

-- deriveSymbol ''ExpBool
-- deriveEquality ''ExpBool

-- instance Render ExpBool
--     where
--         renderSym ExpAnd = "(&&)"
--         renderSym ExpOr = "(||)"
--         renderArgs = renderArgsSmart

-- instance Eval ExpBool
--     where
--         evalSym ExpAnd = (&&)
--         evalSym ExpOr = (||)

-- -- Comparisons

-- data ExpComp sig
--     where
--         ExpLt :: (Num a, Ord a) => ExpComp (a :-> a :-> Full Bool)
--         ExpGt :: (Num a, Ord a) => ExpComp (a :-> a :-> Full Bool)

-- deriveSymbol ''ExpComp
-- deriveEquality ''ExpComp

-- instance Render ExpComp
--     where
--         renderSym ExpLt = "(<)"
--         renderSym ExpGt = "(>)"
--         renderArgs = renderArgsSmart

-- instance Eval ExpComp
--     where
--         evalSym ExpLt = (<)
--         evalSym ExpGt = (>)

-- -- AST

-- type ExpDomain = ExpArith :+: ExpBool :+: ExpComp
-- type Expression a = ASTF ExpDomain a

-- expInt a = Sym (inj $ ExpInt a)
-- expAdd a b = Sym (inj ExpAdd) :$ a :$ b
-- expSub a b = Sym (inj ExpSub) :$ a :$ b
-- expMul a b = Sym (inj ExpMul) :$ a :$ b

-- expAnd a b = Sym (inj ExpAnd) :$ a :$ b
-- expOr a b = Sym (inj ExpOr) :$ a :$ b

-- expLt a b = Sym (inj ExpLt) :$ a :$ b
-- expGt a b = Sym (inj ExpGt) :$ a :$ b

-- -- Examples

-- simpleExpression :: Expression Int
-- simpleExpression = expInt 5

-- complexExpression :: Expression Bool
-- complexExpression = expLt twenty thirty
--     where
--         twenty = expMul (expInt 5) (expInt 4)
--         thirty = expMul (expInt 10) (expInt 3)

-- prettyPrint :: Expression a -> String
-- prettyPrint = render