{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Uniform
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Uniform values
----------------------------------------------------------------------

module Shady.Uniform
  ( (>+>), setUniform,sType
  ) where

-- MTL
import Control.Monad.State

import Graphics.Rendering.OpenGL (GLint,GLfloat,GLenum)
import Graphics.Glew

import TypeNat.Vec hiding (get)

import Shady.Misc (Sink,(>+>))
import Shady.Language.Type (Type(..),VectorT(..),ScalarT(..),Sampler(..))
import Shady.Language.Exp (Pat,V(..))
import Shady.Language.Glom (Glom(..))
import Shady.MechanicsGL
  (GlLoc,GlProgram,GlTextureType,GlTextureUnit,uniformLoc)


-- TODO: take care about missing uniforms.  What does the error look like?


{--------------------------------------------------------------------
    Set a uniform property
--------------------------------------------------------------------}

-- | Set uniform, given max texture units, pattern and GL program
setUniform :: GLint -> Pat a -> GlProgram -> Sink a
setUniform texUnits q prog = evalState (sink q) glTexture0
 where
   sink :: Pat a -> State GlTextureUnit (Sink a)
   sink UnitG      = return (const (return ()))
   sink (pa :* pb) = liftM2 (>+>) (sink pa) (sink pb)
   sink (BaseG v)  = setVar' v
   
   setVar' :: V v -> State GlTextureUnit (Sink v)
   setVar' (V name (SamplerT _)) =
     do unit <- allocUnit
        let loc = fromIntegral (uniformLoc prog name)
        return $ setSampler unit loc
   setVar' (V name (VecT (VectorT n a))) =
     return (setU n a (fromIntegral (uniformLoc prog name)))
   setVar' v = error ("setVar': non-vector/sampler var " ++ show v)
   
   maxUnit = glTexture0 + fromIntegral texUnits - 1
   
   allocUnit :: State GlTextureUnit GlTextureUnit
   allocUnit =
     do unit <- get
        when (unit > maxUnit) $
          error $ "setUniform: exceeded available texture units: "
                  ++ show texUnits
        put (unit + 1)
        return unit

setSampler :: GlTextureUnit -> GlLoc -> Sink (Sampler n)
setSampler unit loc (Sampler n texId) =
  do -- putStrLn $ "setSampler: " ++ show (unit-glTexture0,loc,Sampler n tex)
     glActiveTexture unit
     glBindTexture (textureTy n) (fromIntegral texId)
     glUniform1i loc (fromIntegral (unit-glTexture0))


-- TODO: switch to friendlier HOpenGL interface.  Use TextureObject throughout.

textureTy :: Nat n -> GlTextureType
textureTy (Succ Zero)               = glTexture1D
textureTy (Succ (Succ Zero))        = glTexture2D
textureTy (Succ (Succ (Succ Zero))) = glTexture3D
textureTy n = error $ "textureTy: dimension out of range: " ++ show n


{--------------------------------------------------------------------
    Helpers
--------------------------------------------------------------------}

set1 :: (a -> gla) -> (GlLoc -> gla -> IO ())
      -> GlLoc -> One a -> IO ()
set1 from set l w = set l (from (un1 w))

set2 :: (a -> gla) -> (GlLoc -> gla -> gla -> IO ())
      -> GlLoc -> Two a -> IO ()
set2 from set l w = set l (from a) (from b) where (a,b) = un2 w

set3 :: (a -> gla) -> (GlLoc -> gla -> gla -> gla -> IO ())
      -> GlLoc -> Three a -> IO ()
set3 from set l w = set l (from a) (from b) (from c)
  where (a,b,c) = un3 w

set4 :: (a -> gla) -> (GlLoc -> gla -> gla -> gla -> gla -> IO ())
      -> GlLoc -> Four a -> IO ()
set4 from set l w = set l (from a) (from b) (from c) (from d)
  where (a,b,c,d) = un4 w


setU :: Nat n -> ScalarT a -> GlLoc -> Sink (Vec n a)

setU (Succ Zero)                      Int   = set1 fromI glUniform1i
setU (Succ (Succ Zero))               Int   = set2 fromI glUniform2i
setU (Succ (Succ (Succ Zero)))        Int   = set3 fromI glUniform3i
setU (Succ (Succ (Succ (Succ Zero)))) Int   = set4 fromI glUniform4i

setU (Succ Zero)                      Bool  = set1 fromB glUniform1i
setU (Succ (Succ Zero))               Bool  = set2 fromB glUniform2i
setU (Succ (Succ (Succ Zero)))        Bool  = set3 fromB glUniform3i
setU (Succ (Succ (Succ (Succ Zero)))) Bool  = set4 fromB glUniform4i

setU (Succ Zero)                      Float = set1 fromF glUniform1f
setU (Succ (Succ Zero))               Float = set2 fromF glUniform2f
setU (Succ (Succ (Succ Zero)))        Float = set3 fromF glUniform3f
setU (Succ (Succ (Succ (Succ Zero)))) Float = set4 fromF glUniform4f

setU n t = error ("setU " ++ show (VectorT n t))

-- TODO: Perhaps restructure 'setU' as a 'glUniform' method on IsVector,
-- perhaps with four 'IsScalar' methods 'glUniform1' .. 'glUniform4',
-- invoked by the 'glUniform' methods.  Also a class for converting
-- Haskell types to glUniform types.  Either a fundep or associated type
-- synonym.  Then 'fromI' and 'fromB' would be methods.

-- TODO: Maybe eliminate the type representation types altogether, using
-- classes instead.  Would be more extensible, and perhaps more simply
-- structured.  For instance, 'vSize' (below) would be a method on
-- IsVector, and 'sType' a method on 'IsScalar'.

{--------------------------------------------------------------------
    Misc OpenGL info/convert
--------------------------------------------------------------------}

-- The GL type enum of a scalar
sType :: ScalarT t -> GLenum
sType Bool  = glBool
sType Int   = glInt
sType Float = glFloat

fromI :: Int -> GLint
fromI = fromIntegral

fromF :: Float -> GLfloat
fromF = realToFrac

fromB :: Bool -> GLint
fromB False = 0
fromB True  = 1

-- Or Ivan's clever suggestion:
--   fromB = toEnum . fromEnum

