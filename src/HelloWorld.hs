import Common
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Luminance.Batch
import Graphics.Luminance.Cmd
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.RenderCmd
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Vertex
import Graphics.UI.GLFW

main :: IO ()
main = startup $ \window -> do
  triangle <- createGeometry vertices Nothing Triangle
  program <- sequenceA [createVertexShader vsSource,createFragmentShader fsSource] >>= createProgram_
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer [anySPBatch $ shaderProgramBatch_ program [stdRenderCmd_ triangle]]
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
  , "out vec4 vertexColor;"

  , "vec4 color[3] = vec4[]("
  , "    vec4(1., 0., 0., 1.)"
  , "  , vec4(0., 1., 0., 1.)"
  , "  , vec4(0., 0., 1., 1.)"
  , "  );"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "  vertexColor = color[gl_VertexID];"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "in vec4 vertexColor;"
  , "out vec4 frag;"

  , "void main() {"
  , "  frag = vertexColor;"
  , "}"
  ]

