{-# LANGUAGE TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Run
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some common functionality for compiling & running shaders
----------------------------------------------------------------------

module Shady.Run
  (
    runAnim, time, grid, MeshSize
  -- , runOnce, install, installGrid
  ) where


import Control.Applicative ((<$>),liftA2)

import Data.Time (getCurrentTime,utctDayTime)

import Shady.MechanicsGL (GlIndex)
import Shady.Language.Exp hiding (indices)
import Shady.Misc (Sink,Action)
import qualified Shady.Vec as V


-- GLUT Timer callbacks are one-shot, so re-register each time.  We can re-register
-- first, so try keeping the display rate constant, or re-register second to keep
-- delay constant.  Also, turn off the vsync stuff.


-- | Run an animated shader
runAnim :: Sink Action -> Sink R1 -> IO ()
runAnim start sinkU =
  do t0 <- time
     start (subtract t0 <$> time >>= sinkU)

-- runAnim sinkU = callback (time >>= sinkU) >> mainLoop


-- Get the time since midnight, in seconds
time :: IO R1
time = (vec1 . fromRational . toRational . utctDayTime) <$> getCurrentTime

-- TODO: can I eliminate vec1?  And how about realToFrac?  What's the type
-- of utctDayTime?

{--------------------------------------------------------------------
    Tessellated grid
--------------------------------------------------------------------}

-- | Indices for a collection of triangles covering a grid with given
-- number of columns and rows (in vertices).
gridIndices :: (Enum n, Num n) => (n, n) -> [n]
gridIndices (cols,rows) =
  [ i * rows + j | ll <- lls cols rows , tri <- box ll , (i,j) <- tri ]

-- Lower-lefts
lls :: (Enum a, Num a) => a -> a -> [(a,a)]
lls cols rows = liftA2 (,)  [0 .. cols-2] [0 .. rows-2]

-- CCW triangles in box with given lower-left
box :: Num a => (a,a) -> [[(a,a)]]
box (i,j) = [[ll,lr,ul],[ul,lr,ur]]
 where
   ll = (i  ,j  )
   lr = (i+1,j  )
   ul = (i  ,j+1)
   ur = (i+1,j+1)

-- TODO: Switch to a single triangle, with degeneragte triangles.

-- | (columns,rows) of a mesh
type MeshSize = (GlIndex,GlIndex)

gridVertices :: MeshSize -> [R2]
gridVertices (cols,rows) = liftA2 vert [0 .. cols-1] [0 .. rows-1]
 where
   vert i j = V.vec2 (f i cols) (f j rows)
   f l n = -- fromIntegral l / fromIntegral (n-1) - 0.5
           2 * fromIntegral l / fromIntegral (n-1) - 1


-- | Grid of given dimension
grid :: (GlIndex,GlIndex) -> ([GlIndex], [R2])
grid = liftA2 (,) gridIndices gridVertices
