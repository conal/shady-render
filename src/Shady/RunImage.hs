{-# LANGUAGE TypeOperators, ScopedTypeVariables
           , FlexibleContexts, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.RunImage
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Compile & run a parameterized image
----------------------------------------------------------------------

module Shady.RunImage (runImageB) where

import Shady.Color (HasColor)
import Shady.Misc (Sink)
import Shady.RunSurface (runSurfB')
import Shady.CompileImage (ImageB,eyePos,imSurfB)

-- | Run a 2D animation.  Temporary.
runImageB :: forall c. HasColor c => Sink (ImageB c)
runImageB imb = runSurfB' (2,2) eyePos [imSurfB imb]
