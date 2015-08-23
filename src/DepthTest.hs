import Control.Concurrent
import Control.Monad
import Control.Monad.Except ( MonadError )
import Control.Monad.IO.Class
import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.Resource
import Data.Foldable ( for_ )
import Data.Functor.Contravariant.Divisible
import Graphics.Luminance.Batch
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.RenderCmd
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Shader.Uniform
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
windowTitle = "Test"

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
  (program,colorOffsetU) <- createProgram [vs,fs] $ \uni -> do
    colorU <- uni $ Left "color"
    offsetU <- uni $ Left "offset"
    pure $ divided colorU offsetU
  untilM (liftIO $ windowShouldClose window) $ do
    treatFBBatch $ framebufferBatch defaultFramebuffer
      [anySPBatch . SPBatch program mempty () $
        [
          renderCmd Nothing True colorOffsetU (color0,offset0) triangle 
        , renderCmd Nothing True colorOffsetU (color1,offset1) triangle 
        , renderCmd Nothing True colorOffsetU (color2,offset2) triangle 
        ]
      ]
    liftIO $ do
      pollEvents
      swapBuffers window
      threadDelay 50000

color0,color1,color2 :: (Float,Float,Float)
color0 = (1,0,0)
color1 = (0,1,0)
color2 = (0,0,1)

offset0,offset1,offset2 :: (Float,Float)
offset0 = (-0.25,0)
offset1 = (0.25,0)
offset2 = (0,0.25)

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

  , "uniform vec2 offset;"

  , "void main() {"
  , "  gl_Position = vec4(co + offset, 0., 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "#version 450 core"

  , "out vec4 frag;"

  , "uniform vec3 color;"

  , "void main() {"
  , "  frag = vec4(color, 1.);"
  , "}"
  ]

untilM :: (Monad m) => m Bool -> m b -> m ()
untilM predicate a = go
  where
    go = do
      p <- predicate
      if p then pure () else a >> go
