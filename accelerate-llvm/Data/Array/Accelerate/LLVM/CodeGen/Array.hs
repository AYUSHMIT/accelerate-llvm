{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Array
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Array (

  readArray,  readVolatileArray,
  writeArray, writeVolatileArray,

) where

import Control.Applicative
import Foreign.Ptr
import Prelude                                                          hiding ( read )

import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Instruction

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar



-- | Read a value from an array at the given index
--
readArray :: forall sh e. IRArray (Array sh e) -> IR Int -> CodeGen (IR e)
readArray (IRArray _ (IR adata)) (op integralType -> ix) =
  IR <$> readArrayData NonVolatile ix (eltType (undefined::e)) adata

-- | Read a value from a volatile array at the given index
--
readVolatileArray :: forall sh e. IRArray (Array sh e) -> IR Int -> CodeGen (IR e)
readVolatileArray (IRArray _ (IR adata)) (op integralType -> ix) =
  IR <$> readArrayData Volatile ix (eltType (undefined::e)) adata

readArrayData :: Volatile -> Operand Int -> TupleType t -> Operands t -> CodeGen (Operands t)
readArrayData volatile ix = read
  where
    read :: TupleType t -> Operands t -> CodeGen (Operands t)
    read UnitTuple          OP_Unit        = return OP_Unit
    read (PairTuple t2 t1) (OP_Pair a2 a1) = OP_Pair <$> read t2 a2 <*> read t1 a1
    read (SingleTuple t)   (op' t -> arr)  = ir' t   <$> readArrayPrim t volatile (PtrOperand arr) ix

readArrayPrim :: ScalarType e -> Volatile -> Operand (Ptr e) -> Operand Int -> CodeGen (Operand e)
readArrayPrim t volatile arr i = do
  ptr   <- instr' $ GetElementPtr arr [i]
  v     <- instr' $ Load t volatile ptr
  return v


-- | Write a value into an array at the given index
--
writeArray :: forall sh e. IRArray (Array sh e) -> IR Int -> IR e -> CodeGen ()
writeArray (IRArray _ (IR adata)) (op integralType -> ix) (IR val) =
  writeArrayData NonVolatile ix (eltType (undefined::e)) adata val

-- | Write a value into a volatile array at the given index
--
writeVolatileArray :: forall sh e. IRArray (Array sh e) -> IR Int -> IR e -> CodeGen ()
writeVolatileArray (IRArray _ (IR adata)) (op integralType -> ix) (IR val) =
  writeArrayData Volatile ix (eltType (undefined::e)) adata val


writeArrayData :: Volatile -> Operand Int -> TupleType t -> Operands t -> Operands t -> CodeGen ()
writeArrayData volatile ix = write
  where
    write :: TupleType e -> Operands e -> Operands e -> CodeGen ()
    write UnitTuple          OP_Unit         OP_Unit        = return ()
    write (PairTuple t2 t1) (OP_Pair a2 a1) (OP_Pair v2 v1) = write t1 a1 v1 >> write t2 a2 v2
    write (SingleTuple t)   (op' t -> arr)  (op' t -> val)  = writeArrayPrim volatile (PtrOperand arr) ix val

writeArrayPrim :: Volatile -> Operand (Ptr e) -> Operand Int -> Operand e -> CodeGen ()
writeArrayPrim volatile arr i v = do
  ptr   <- instr' $ GetElementPtr arr [i]
  _     <- instr' $ Store volatile ptr v
  return ()

