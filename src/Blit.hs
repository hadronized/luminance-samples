import Common
import Control.Monad.IO.Class
import Graphics.Luminance.Batch
import Graphics.Luminance.Cmd
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

main :: IO ()
main = startup $ \window -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  fb :: Framebuffer RW RGBA32F () <- createFramebuffer windowW windowH 1
  program <- createProgram_ [vs,fs]
  untilM (liftIO $ windowShouldClose window) $ do
    _ <- runCmd $ do
      _ <- draw $ framebufferBatch fb 
        [anySPBatch $ shaderProgramBatch program mempty () [renderCmd Nothing True mempty () triangle]]
      blit fb defaultFramebuffer 0 0 windowW windowH 0 0 windowW windowH BlitColor Linear
    endFrame window

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
