{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Attribute
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Setting attributes.  An attribute is a stream of values named and
-- described by a pattern.  Attributes can be uniform vectors but also
-- pairs of attributes, as well as ().  Not really a single attribute, but
-- rather a conglomeration of all properties of a vertex.
-- 
-- I hope that glomming all properties into one value will be convenient,
-- as well as eliminating the possibility of mismatching lengths of
-- separated attributes.
-- 
-- When set, these conglomerates get split into separate vector-valued
-- attributes as required by GLSL.
-- 
-- TODO: Replace the name "attribute" with something more descriptive.
-- Maybe "vertex" or "vertex description".
----------------------------------------------------------------------

module Shady.Attribute
  ( setAttribute
  -- * Experimental
  , setAttribute', SinkUse
  ) where

import Graphics.Rendering.OpenGL hiding (Shader,Program,Index,Sink,Int,Bool,Float)

import Control.Applicative (liftA2)

import Shady.MechanicsGL hiding (Attributes, mkAttribute)

import TypeUnary.Vec (natToZ)

import Shady.Language.Type (VectorT(..),Type(..))
import Shady.Language.Glom (Glom(..))
import Shady.Uniform (sType)
import Shady.Language.Exp (Pat,V(..))
import Shady.Misc (Sink,Unop)

setAttribute :: Pat a -> GlProgram -> Sink [a]
setAttribute p prog = sink p
 where
   sink :: Pat a -> Sink [a]
   sink UnitG      = const (return ())
   sink (pa :* pb) = sink pa >+> sink pb
   sink (BaseG v)  = vsink prog v

infixr 1 >+>
-- Combine sinks
(>+>) :: Sink [a] -> Sink [b] -> Sink [(a,b)]
(sa >+> sb) ps = sa as >> sb bs where (as,bs) = unzip ps


-- Sink for vector lists
vsink :: GlProgram -> V v -> Sink [v]
vsink prog (V name (VecT ty)) = \ xs ->
  do vbo <- mkBuffer
     arr <- listArr xs
     _ <- createVBO ArrayBuffer vbo arr (lsize xs)
     useVar vbo (fromIntegral (attributeLoc prog name)) ty
vsink _ v = error ("vsink: non-vector variable " ++ show v)

-- TODO: separate sinking from using

useVar :: BufferObject -> GlLoc' -> VectorT n a -> IO ()
useVar vbo loc (VectorT n a) = useVBO (natToZ n) (sType a) loc vbo


{--------------------------------------------------------------------
    Now create the VBO and returns the action to use it.
--------------------------------------------------------------------}

-- | Stash away some information and make an action to activate it later.
type SinkUse z = z -> IO (Unop (IO ()))

-- Or maybe
-- 
--    type SinkUse z = z -> IO (forall a. Unop (IO a))

setAttribute' :: Pat a -> GlProgram -> SinkUse [a]
setAttribute' p prog = sink p
 where
   sink :: Pat a -> SinkUse [a]
   sink UnitG      = const (return id)
   sink (pa :* pb) = sink pa >++> sink pb
   sink (BaseG v)  = vsink' prog v

infixr 1 >++>

(>++>) :: SinkUse [a] -> SinkUse [b] -> SinkUse [(a,b)]
(sa >++> sb) ps = liftA2 (.) (sa as) (sb bs)
  where (as,bs) = unzip ps

vsink' :: GlProgram -> V v -> SinkUse [v]
vsink' prog (V name (VecT ty)) = \ xs ->
  do vbo <- mkBuffer
     arr <- listArr xs
     _ <- createVBO ArrayBuffer vbo arr (lsize xs)
     return (usingVar ty (fromIntegral (attributeLoc prog name)) vbo)
vsink' _ v = error ("vsink: non-vector variable " ++ show v)


usingVar :: VectorT n a -> GlLoc' -> BufferObject -> Unop (IO c)
usingVar (VectorT n a) = usingVBO (natToZ n) (sType a)
