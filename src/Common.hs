module Common where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.UI.GLFW
import Prelude hiding ( init )
import System.IO ( hPutStrLn, stderr )

type App = ExceptT AppError (ResourceT IO)

newtype AppError = AppError String deriving (Eq,Show)

instance HasFramebufferError AppError where
  fromFramebufferError (IncompleteFramebuffer s) = AppError s

instance HasStageError AppError where
  fromStageError (CompilationFailed s) = AppError s

instance HasProgramError AppError where
  fromProgramError e = case e of
    LinkFailed s -> AppError s
    InactiveUniform u -> AppError (show u)

windowW,windowH :: (Num a) => a
windowW = 800
windowH = 600

windowTitle :: String
windowTitle = "luminance-sample"

startup :: (Window -> App ()) -> IO ()
startup app = do
  _ <- init
  windowHint (WindowHint'Resizable False)
  windowHint (WindowHint'ContextVersionMajor 4)
  windowHint (WindowHint'ContextVersionMinor 5)
  windowHint (WindowHint'OpenGLForwardCompat False)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  window <- createWindow windowW windowH windowTitle Nothing Nothing
  makeContextCurrent window
  case window of
    Just window' -> do
      swapInterval 1
      (runResourceT . runExceptT . app) window' >>= either print (const $ pure ())
      destroyWindow window'
    Nothing -> hPutStrLn stderr "unable to create window; please check your hardware support OpenGL4.5"
  terminate

endFrame :: (MonadIO m) => Window -> m ()
endFrame window = liftIO $ do
  pollEvents
  swapBuffers window
  threadDelay 50000

untilM :: (Monad m) => m Bool -> m b -> m ()
untilM predicate a = go
  where
    go = do
      p <- predicate
      if p then pure () else a >> go
