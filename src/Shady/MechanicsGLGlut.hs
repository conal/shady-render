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

module Shady.MechanicsGLGlut (shadyInit) where


import Graphics.Rendering.OpenGL hiding (Shader,Program,Index,Sink)

import Graphics.UI.GLUT
  ( getArgsAndInitialize, createWindow, windowSize, initialDisplayMode
  , DisplayMode(DoubleBuffered,RGBAMode,WithDepthBuffer,WithAlphaComponent)
  , ActionOnWindowClose(MainLoopReturns)
  , displayCallback, actionOnWindowClose, swapBuffers
  , mainLoop, addTimerCallback
  )

import Graphics.Glew
import Shady.Misc (Sink,Action)

-- | Initialize Shady and return a wrapper for a drawing command.
-- Supplying that drawing command will cause it to be invoked regularly.
shadyInit :: String -> IO (Sink Action)
shadyInit title =
  do putStrLn "getArgsAndInitialize"
     _ <- getArgsAndInitialize
--      putStrLn "IL.init"
--      IL.init
     putStrLn "createWindow"
     _ <- createWindow title
     windowSize $= Size 600 600
     -- fullScreen
     initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer, WithAlphaComponent ]
     putStrLn "depthFunc"
     depthFunc $= Just Less  -- defaults to Nothing / Just Always
     putStrLn "glewInit"
     _ <- glewInit  -- must come after createWindow -- otherwise segfault
     putStrLn "survived glewInit"
     -- Experimenting with transparency.  OpenGL (really, z-buffering)
     -- doesn't handle transparency sensibly.  Rendering is order-dependent.
     --   blend $= Enabled >> blendFunc $= (SrcAlpha, One)
     -- Hm.  The transparency in lesson08 in Lispy's nehe translations
     -- looks good.  Oh.  It's a hack, turning off depth-writing.  See
     -- <http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=08>
     -- End of experiments
     putStrLn "glEnableVSync"
     glEnableVSync True
     -- Hm.  These two queries tell me "2.1.2 NVIDIA 180.11, 1.20 NVIDIA
     -- via Cg compiler", but I am getting hyperbolic trig functions.
     -- What gives?  Oh -- maybe HOpenGL was compiled against OpenGL
     -- 2.1.2 & 1.20, although I'm really running 3.0 and 1.20.  I don't know.
     -- get glVersion >>= putStr . (++ ", ")
     -- get shadingLanguageVersion >>= putStrLn 
     displayCallback $= return ()   -- real update via timer.
     catch (actionOnWindowClose $= MainLoopReturns)
           (const (return ()))
     putStrLn "finished shadyInit"
     return $ timedDisplay . display

-- From MechanicsGLGtk (in shady-tv)

-- timedDisplay :: Gtk.Window -> GtkGL.GLDrawingArea -> Action -> IO ()
-- timedDisplay window canvas render = 
--   do timeout <- Gtk.timeoutAddFull (display canvas render >> return True)
--                                    Gtk.priorityDefaultIdle period
--      Gtk.onDestroy window (Gtk.timeoutRemove timeout >> Gtk.mainQuit)
--      Gtk.mainGUI
--  where
--    period = 1000 `div` 60   -- milliseconds

timedDisplay :: Sink Action
timedDisplay disp =
  do timedCallback period disp
     mainLoop
 where
  period = 1000 `div` 60   -- milliseconds

display :: Sink Action
display render =
  do clear [ColorBuffer, DepthBuffer]
     render
     glWaitVSync
     finish
     -- flush
     swapBuffers

timedCallback :: Int -> Sink Action
timedCallback ms act = loop
 where
   loop = addTimerCallback ms (act >> loop)

-- Alternatively:
-- 
--   idleCallback $= Just act
