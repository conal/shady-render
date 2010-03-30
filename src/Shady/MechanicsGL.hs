-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.MechanicsGL
-- Copyright   :  (c) Conal Elliott
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some utilities for shader generation
----------------------------------------------------------------------

module Shady.MechanicsGL
  ( GlShaderType, GlTextureType
  , GlProgram, GlShader, GlLoc, GlLoc', GlIndex, GlTexture
  , GlFBO, GlTextureUnit
  , WrapIO
  , Attributes(..), mkAttribute
  , mkBuffer, listArr, lsize, createVBO, useVBO, usingVBO
  , mkIndices, setupShader
  , checkErrors, setupMatrices
  , attributeLoc, uniformLoc
  , drawStrip, drawTriangles
  , glMaxTextureUnits
  -- , loadTexture
  -- * Re-export
  , glUseProgram
  -- * toolkit-specific (e.g., GLUT vs gtk2hs)
  , EyePos, shadyInit
  ) where

-- TODO: prune exports and definitions.  I expect a bunch of old stuff can
-- be removed.

import Control.Applicative ((<$>))
import Control.Monad (unless)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (bracket_)
import Data.List (isSuffixOf)

import Data.Array.Storable

import Foreign.Ptr (nullPtr)
import Foreign.Storable

import Graphics.Rendering.OpenGL hiding (Shader,Program,Index)
import Graphics.Glew

-- import qualified Shady.IL as IL
import Shady.Misc (Unop,Action,EyePos)

import Shady.MechanicsGLGlut
-- import Shady.MechanicsGLGtk

{-

TODO: 

* Make this module be about a nicely typed interface to the mechanics.
* Rename module.  Maybe "Plumbing".
* Comments for all top-level definitions
* Prune exports, especially the untyped GlProgram, etc.
* Re-visit what's here and what's in Uniform.

-}

type GlShaderType  = GLenum
type GlTextureType = GLenum

type GlProgram     = GLuint
type GlShader      = GLuint
type GlLoc         = GLint
type GlLoc'        = GLuint
type GlIndex       = GLushort
type GlTexture     = GLuint
type GlFBO         = GLuint
type GlTextureUnit = GLenum

type WrapIO c = Unop (IO c)

-- Strangely, glUniform1i etc want locations as GLint, while
-- glGetUniformLoc wants them as GLuint


-- Make a single BufferObject
mkBuffer :: IO BufferObject
mkBuffer = head <$> genObjectNames 1

-- The total storage size of a list of elements.
lsize :: (Integral n, Storable a) => [a] -> n
lsize as = fromIntegral (length as * sizeOf (head as))

-- | Make connectivity information.  Return an action that selects the indices.
mkIndices :: [GlIndex] -> IO Action
mkIndices ixs =
  do ib <- mkBuffer
     is <- listArr ixs
     createVBO ElementArrayBuffer ib is (lsize ixs)

-- TODO: why doesn't the index buffer get saved explicitly?  Will there be
-- trouble with multiple meshes?  I think there will.  

-- Convert a list to an array
listArr :: (MArray a e m) => [e] -> m (a Int e)
listArr xs = newListArray (1, length xs) xs


createVBO :: BufferTarget -> BufferObject -> StorableArray i e
          -> GLsizeiptr -> IO Action
createVBO btype buffer arr size =
  do select
     withStorableArray
         arr
         (\ptr -> bufferData btype $= (size, ptr, StaticDraw))
     return select
 where
   select = bindBuffer btype $= Just buffer


-- | Use a vertex buffer object of the given storage size, GL-type, and
-- location/index.
useVBO :: GLint -> GLenum -> GlLoc' -> BufferObject -> IO ()
useVBO gls glt loc vbo =
  do glEnableVertexAttribArray loc
     attribPointer loc gls glt vbo

-- TODO: Phase out useVBO, replacing with usingVBO

-- | Like glVertexAttribPointer but with explicit VBO argument.
attribPointer :: GlLoc' -> GLint -> GLenum -> BufferObject -> IO ()
attribPointer loc gls glt vbo =
  do bindBuffer ArrayBuffer $= Just vbo
     glVertexAttribPointer loc gls glt 0 0 nullPtr

-- Or with explicit clean-up:
-- 
--   attribPointer loc gls glt vbo =
--     bindingBuffer ArrayBuffer vbo $
--       glVertexAttribPointer loc gls glt 0 0 nullPtr
-- 
--   bindingBuffer :: BufferTarget -> BufferObject -> WrapIO c
--   bindingBuffer buffTy vbo =
--     bracket_ (bindBuffer buffTy $= Just vbo)
--              (bindBuffer buffTy $= Nothing)

-- | Use a vertex buffer object of the given storage size, GL-type, and
-- location/index.
usingVBO :: GLint -> GLenum -> GlLoc' -> BufferObject -> WrapIO c
usingVBO gls glt loc vb io =
  enablingVertexAttribArray loc $
    attribPointer loc gls glt vb >> io

enablingVertexAttribArray :: GlLoc' -> WrapIO c
enablingVertexAttribArray vloc =
  bracket_ (glEnableVertexAttribArray vloc)
           (glDisableVertexAttribArray vloc)

-- | Array of vertex attribute values
newtype Attributes a = Attributes BufferObject

-- | Make an array buffer out of a list of values.  Return a means of
-- connecting the array buffer to an named attribute in a shader program.
mkAttribute :: Storable a => [a] -> IO (Attributes a)
mkAttribute xs =
  do vbo <- mkBuffer
     arr <- listArr xs
     _ <- createVBO ArrayBuffer vbo arr (lsize xs)
     return (Attributes vbo)

-- | Make a shader of given type from a source string
mkShader :: GlShaderType -> String -> IO GlShader
mkShader ty src = 
  do sh <- glCreateShader ty
     loadShader sh src
     return sh

mkProgram :: [GlShader] -> IO GlProgram
mkProgram shaders =
  do p <- glCreateProgram
     mapM_ (glAttachShader p) shaders
     glLinkProgram p
     return p

-- | compile and link a vertex and fragment shader from strings.
setupShader :: String -> String -> IO GlProgram
setupShader vshader fshader =
  do vsh <- mkShader glVertexShader   vshader
     fsh <- mkShader glFragmentShader fshader
     p   <- mkProgram [vsh,fsh]
     return p

-- TODO: consider finalizers and safePerfomIO for mkShader, mkProgram, and
-- setupShader.  I think they're referentially transparent.  First, wrap
-- the GlShader type as an abstract type, so one cannot examine the inner
-- ints.
--
-- When I unsafePerformIO'd setupShader, I got an error after the first run.
-- Perhaps something about reusing resources after a glewInit.

loadShader :: GlShader -> String -> IO ()
loadShader shader src =
  do glLoadShader shader src
     glCompileShader shader
     glGetShaderLog shader >>= reportErrors "Shader"

checkErrors :: IO ()
checkErrors = show <$> get errors >>= reportErrors "General"

reportErrors :: String -> String -> IO ()
reportErrors label str =
  unless (null errs) $ putStrLn (label ++ " error Log:\n" ++ errs)
 where
   errs = nonbogus str

-- On Mac OS 10.5, I get shader error messages of this form:
bogusMsg :: String -> Bool
bogusMsg = isSuffixOf "Selection Node found in constant constructor"

nonbogus :: String -> String
nonbogus = unlines . filter (not . bogusMsg) . lines

-- | Establish default viewing and modeling transforms.  Looking at the XY
-- plane from 2 units back, with Y up.
setupMatrices :: EyePos -> IO ()
setupMatrices (ex,ey,ez) =
  do -- putStrLn "matrixMode Projection"
     matrixMode $= Projection >> loadIdentity
     -- putStrLn "perspective"
     perspective 53.0 1 0.1 100
     -- putStrLn "matrixMode Modelview"
     matrixMode $= Modelview 0 >> loadIdentity
     -- lookAt takes eye, center, up.
     lookAt (Vertex3 (f2d ex) (f2d ey) (f2d ez)) (Vertex3 0 0 0) (Vector3 0 1 0)
 where
   f2d :: Float -> GLdouble
   f2d = realToFrac
         -- fromRational . toRational   -- TODO: remember & use the more direct route

{-
perspective' :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
-- perspective' = perspective

perspective' fovy aspect near far =
  frustum (-halfWidth) halfWidth (-halfHeight) halfHeight near far
 where
    halfHeight = near * tan (fovy * pi / 360)
    halfWidth  = halfHeight * aspect;
-}

-- TODO: For 2D, use orthogonal projection and no StencilBuffer or DepthBuffer

-- | Extract the location of a vertex attribute or uniform.
attributeLoc :: GlProgram -> String -> GlLoc
attributeLoc = (fmap.fmap) unsafePerformIO glGetAttribLoc

-- | Extract the location of a vertex attribute or uniform.
uniformLoc :: GlProgram -> String -> GlLoc
uniformLoc = (fmap.fmap) unsafePerformIO glGetUniformLoc

-- uniformLoc p s = unsafePerformIO $
--                  do l <- glGetUniformLoc p s
--                     putStrLn $ "uniformLoc " ++ show (p,s,l)
--                     return l

-- | Draw a tri-strip, given the number of vertices.  Assumes the
-- connectivity and vertices are already set up.
drawStrip :: NumArrayIndices -> IO ()
drawStrip n = drawElements TriangleStrip n UnsignedShort nullPtr

-- | Draw a tri-strip, given the number of vertices.  Assumes the
-- connectivity and vertices are already set up.
drawTriangles :: NumArrayIndices -> IO ()
drawTriangles n = drawElements Triangles n UnsignedShort nullPtr


-- Quads (and polygons) are deprecated in opengl 3.  When I use
-- TriangleFan, I sometimes get a missing ray emanating from the origin at
-- pi/4.  Not so with Quad or Triangles.  Haven't tried TriangleStrip.


-- | Number of texture units available
glMaxTextureUnits :: IO GLint
glMaxTextureUnits = fromIntegral <$> get maxCombinedTextureImageUnits

{-
-- | Load a texture from an image file
loadTexture :: FilePath -> IO GlTexture
loadTexture fname = do im  <- IL.loadImage fname
                       IL.imageSize im >>= print  -- 1 x 1 ?!
                       err <- IL.showErrors
                       if err then
                          return 0
                        else do tex <- setupTexture
                                IL.bindImage im         -- redundant?
                                IL.glTexImage 0         -- fill in level 0
                                -- this glGenerateMipmapEXT call crashes,
                                -- so comment out, along with the setting
                                -- in setupTexture.  Revisit when I see
                                -- non-mipmapped textures working.  Maybe
                                -- the glTexImage call isn't succeeding
                                -- 
                                -- glGenerateMipmapEXT glTexture2D -- and then the rest
                                return tex

setupTexture :: IO GLuint
setupTexture =
  do tex <- glGenTexture
     glBindTexture glTexture2D tex
     glTexParameteri glTexture2D glTextureWrapS glRepeat -- glClampToEdge
     glTexParameteri glTexture2D glTextureWrapT glRepeat -- glClampToEdge
     glTexParameteri glTexture2D glTextureMagFilter glLinear
     -- See above
     -- glTexParameteri glTexture2D glTextureMinFilter glLinearMipmapLinear
     return tex

-}
