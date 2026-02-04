{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}

module Data.CallStack (
  HasCallStack
, CallStack
, SrcLoc(..)
, callStack
) where

import Data.SrcLoc

#if MIN_VERSION_base(4,8,1)
import qualified GHC.Stack as GHC
#endif

#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (HasCallStack)
#elif MIN_VERSION_base(4,8,1)
type HasCallStack = (?callStack :: GHC.CallStack)
#else
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
#endif

type CallStack = [(String, SrcLoc)]

callStack :: HasCallStack => CallStack
#if MIN_VERSION_base(4,9,0)
callStack = drop 1 $ GHC.getCallStack GHC.callStack
#elif MIN_VERSION_base(4,8,1)
callStack = drop 2 $ GHC.getCallStack ?callStack
#else
callStack = []
#endif
