import Control.Concurrent
import Control.Monad
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.Resource
import Data.Foldable ( for_ )
import Graphics.Luminance.Batch
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.RenderCmd
import Graphics.Luminance.Pixel as L
import Graphics.Luminance.RW
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Texture
import Graphics.Luminance.Vertex
import Graphics.UI.GLFW
import Prelude hiding ( init )

data AppError = AppError String deriving (Eq,Show)

instance HasFramebufferError AppError where
  fromFramebufferError e = AppError (show e)

instance HasStageError AppError where
  fromStageError e = AppError (show e)

instance HasProgramError AppError where
  fromProgramError e = AppError (show e)

windowW,windowH :: (Num a) => a
windowW = 800
windowH = 600

windowTitle :: String
windowTitle = "Blit"

main :: IO ()
main = do
  _ <- init
  windowHint (WindowHint'Resizable False)
  windowHint (WindowHint'ContextVersionMajor 4)
  windowHint (WindowHint'ContextVersionMinor 5)
  windowHint (WindowHint'OpenGLForwardCompat False)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  window <- createWindow windowW windowH windowTitle Nothing Nothing
  makeContextCurrent window
  for_ window $ \window' -> do
    (runResourceT . runExceptT . app) window' >>= either print (const $ pure ())
    destroyWindow window'
  terminate

app :: (MonadError AppError m,MonadIO m,MonadResource m) => Window -> m ()
app window = do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (fb :: Framebuffer RW RGBA32F (),_,_) <- createFramebuffer windowW windowH 1
  program <- createProgram_ [vs,fs]
  untilM (liftIO $ windowShouldClose window) $ do
    treatFBBatch $ framebufferBatch fb 
      [anySPBatch $ SPBatch program mempty () [renderCmd Nothing True mempty () triangle]]
    blit fb defaultFramebuffer 0 0 windowW windowH 0 0 windowW windowH BlitColor Linear
    liftIO $ do
      pollEvents
      swapBuffers window
      threadDelay 50000

vertices :: [V 2 Float]
vertices =
  [
    V2 (-0.5) (-0.5)
  , V2 0 0.5
  , V2 0.5 (-0.5)
  ]

vsSource :: String
vsSource = unlines
  [
    "#version 450 core"
  
  , "in vec2 co;"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "#version 450 core"

  , "out vec4 frag;"

  , "void main() {"
  , "  frag = vec4(1., 0., 0., 1.);"
  , "}"
  ]

untilM :: (Monad m) => m Bool -> m b -> m ()
untilM predicate a = go
  where
    go = do
      p <- predicate
      if p then pure () else a >> go
