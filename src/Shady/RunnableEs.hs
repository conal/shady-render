{-# LANGUAGE TypeOperators, ScopedTypeVariables, TypeFamilies
           , FlexibleContexts, ExistentialQuantification, GADTs
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.RunnableEs
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generate and compile vertex and fragment shaders.  Unlike
-- "Shady.RunnableE", this version allows a looser structure to
-- the inputs & outputs of shaders, according to 'FromE'.  You can use
-- the types and 'compile' in this module, or just 'shaders', along with
-- "Shady.RunnableE", e.g., @compile (shaders sh)@.
----------------------------------------------------------------------

module Shady.RunnableEs (compile) where

import Text.PrettyPrint.Leijen.DocExpr (HasExpr)

import Shady.Language.Exp
import Shady.CompileEs (ShaderVF, shaders)
import qualified Shady.RunnableE as R
import Shady.Misc (Sink)


-- | Compile a parameterized shader program.  Set up a static (for now)
-- vertex mesh, and give a sink for setting uniforms and rendering.
compile :: forall u' a' u a.
           ( FromE u', u ~ ExpT u', FromE a', a ~ ExpT a') =>
           ( HasType a, HasExpr a, HasType u, HasExpr u ) =>
           (u' -> ShaderVF a') -> IO () -> [a] -> IO (Sink u)
compile = R.compile . shaders

