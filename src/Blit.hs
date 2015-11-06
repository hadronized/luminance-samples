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
main = startup $ \window mainLoop -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  fb :: Framebuffer RW RGBA32F () <- createFramebuffer windowW windowH 1
  program <- createProgram_ [vs,fs]
  mainLoop $ do
    _ <- runCmd $ do
      _ <- draw $ framebufferBatch fb 
        [anySPBatch $ shaderProgramBatch_ program [stdRenderCmd_ triangle]]
      blit fb defaultFramebuffer 0 0 windowW windowH 0 0 windowW windowH BlitColor Linear
    endFrame window

vertices :: [V 2 Float]
vertices =
  [
    vec2 (-0.5) (-0.5)
  , vec2 0 0.5
  , vec2 0.5 (-0.5)
  ]

vsSource :: String
vsSource = unlines
  [
    "in vec2 co;"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "out vec4 frag;"

  , "void main() {"
  , "  frag = vec4(1., 0., 0., 1.);"
  , "}"
  ]
