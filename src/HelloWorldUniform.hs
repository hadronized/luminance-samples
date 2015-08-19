import Control.Concurrent
import Control.Monad.Except ( MonadError )
import Control.Monad.IO.Class
import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.Resource
import Data.Foldable ( for_, traverse_ )
import Graphics.Luminance.Batch
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Shader.Uniform
import Graphics.UI.GLFW
import Prelude hiding ( init )

data AppError = AppError String deriving (Eq,Show)

instance HasFramebufferError AppError where
  fromFramebufferError (IncompleteFramebuffer s) = AppError s

instance HasStageError AppError where
  fromStageError (CompilationFailed s) = AppError s

instance HasProgramError AppError where
  fromProgramError (LinkFailed s) = AppError s

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
  for_ window $ \window' ->
    (runResourceT . runExceptT . app) window' >>= either print (const $ pure ())
  terminate

app :: (MonadError AppError m,MonadIO m,MonadResource m) => Window -> m ()
app window = do
  --framebuffer :: Framebuffer W RGB32F () <- createFramebuffer windowW windowH 1
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (program,colorsU :: Maybe (U [(Float,Float,Float)])) <- createProgram [vs,fs] (\f -> f $ Left "colors")
  treatFBBatch $ FBBatch defaultFramebuffer [SPBatch program (traverse_ (@= colors) colorsU) $ [triangle]]
  liftIO $ do
    putStrLn "done!"
    swapBuffers window
    threadDelay 4000000

colors :: [(Float,Float,Float)]
colors = [(1,0,0),(0,1,0),(0,0,1)]

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
  , "out vec4 vertexColor;"

  , "uniform vec3[] colors;"

  , "vec3 color[3] = vec3[]("
  , "    colors[0]"
  , "  , colors[1]"
  , "  , colors[2]"
  , "  );"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "  vertexColor = vec4(color[gl_VertexID], 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "#version 450 core"

  , "in vec4 vertexColor;"
  , "out vec4 frag;"


  , "void main() {"
  , "  frag = vertexColor;"
  , "}"
  ]
