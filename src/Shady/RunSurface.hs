{-# LANGUAGE TypeOperators, ScopedTypeVariables
           , FlexibleContexts, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.RunSurface
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Assemble shaders and display an image
----------------------------------------------------------------------

module Shady.RunSurface
  (
   runSurfB, runSurfB',compileSurf
  , FullSurf, EyePosE  -- re-export
  ) where

-- TODO: trim exports

import Data.Monoid (mconcat)

import Data.Derivative (pureD)

import Text.PrettyPrint.Leijen.DocExpr (HasExpr)

import qualified TypeNat.Vec as V

import Shady.Language.Exp hiding (indices)
import Shady.Misc (Sink,Action)

import Shady.MechanicsGL
  (drawTriangles,shadyInit,setupMatrices,mkIndices,EyePos)
import Shady.RunnableEs (compile)
import Shady.Run

import Shady.CompileSurface (SurfB,EyePosE,FullSurf,wrapSurf)

-- | Run a 3D animation, given a mesh size & eye position
runSurfB' :: MeshSize -> EyePos -> Sink [SurfB]
runSurfB' size eyePos surfBs =
  compileScene size eyePos (map (. pureD) surfBs) >>= uncurry runAnim

-- | Run a 3D animation.  Temporary.  Replace with a more general version
-- version that puts up a custom GUI.
runSurfB :: EyePos -> Sink [SurfB]
runSurfB = runSurfB' (100,100)  --  strange!  somewhere over 105 i lose much of the mesh.

-- | Compile a single surface, given size (columns,rows), and a static mesh.
compileSurf :: (FromE u', u ~ ExpT u', HasType u, HasExpr u) =>
               MeshSize -> [R2] -> EyePosE -> Action -> (u' -> FullSurf) -> IO (Sink u)
compileSurf (cols,rows) vertices eyePos prep s = compile (wrapSurf eyePos s) draw vertices
 where
   draw = -- putStrLn "drawTriangles" >>
          do prep
             drawTriangles (fromIntegral (6 * (cols-1) * (rows-1)))


-- | Compile a scene full of parameterized 'FullSurf's, given
-- (columns,rows) for a static mesh.  The resulting sink drives redisplay,
-- using the current values of all uniform parameters.
compileScene :: (FromE u', u ~ ExpT u', HasType u, HasExpr u) =>
                MeshSize -> EyePos -> [u' -> FullSurf] -> IO (Sink Action, Sink u)
compileScene size eyePos@(ex,ey,ez) fs =
  do start <- shadyInit "Transmission from the Council of All Worlds"
     setupMatrices eyePos
     setI <- mkIndices indices
     sinkUs <- mapM (compileSurf size vertices eyePosE setI) fs
     return (start, mconcat sinkUs)
 where
   eyePosE :: EyePosE
   eyePosE = pureE (V.vec3 ex ey ez)
   (indices,vertices) = grid size


-- TODO: Eliminate the Action sink return from compileScene.  Then remove
-- the fmap here.


