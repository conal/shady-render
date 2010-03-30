{-# LANGUAGE TypeOperators, ScopedTypeVariables, ExistentialQuantification
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.RunnableE
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generate and compile vertex and fragment shaders.
-- 
-- In this version, shader programs are represented by functions function
-- a single expression to a single expression.  See also RunnableEs, which
-- allows functions between more flexible representations.
----------------------------------------------------------------------

module Shady.RunnableE (compile) where

import Text.PrettyPrint.Leijen.DocExpr (HasExpr)

import Shady.Misc (Sink)
import Shady.Language.Exp (HasType,(:=>))

import Shady.MechanicsGL (setupShader,glUseProgram,glMaxTextureUnits)
import Shady.Uniform (setUniform)
import Shady.Attribute (setAttribute)

import Shady.CompileE

-- | Compile a parameterized shader program.  Set up a static (for now)
-- vertex mesh, and give a sink for setting uniforms and rendering.
compile :: (HasType a, HasExpr a, HasType u, HasExpr u) =>
           (u :=> ShaderVF a) -> IO () -> [a] -> IO (Sink u)
compile shf draw as =
  do -- print (pretty g)
     p     <- setupShader vsh fsh
     units <- glMaxTextureUnits
     let useProg = glUseProgram        p
         setA    = setAttribute     pa p
         setU    = setUniform units pu p
     useProg
     setA as
     return $ \ u -> useProg >> setU u >> draw
 where
   GLSL vsh fsh pu pa = shaderProgram shf


-- TODO: switch from Sink [a] to Sink (Vbos a), so that the [a] -> Vbos
-- conversion can be done up front.  Vbos = Glom Vbo.  Then simplify the
-- signature to Vbos a -> u -> IO ().

-- For now I'm wiring in a fixed mesh.


