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
import Graphics.Luminance.Shader.Uniform
import Graphics.Luminance.Vertex
import Graphics.UI.GLFW

main :: IO ()
main = startup $ \window -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (program,colorsU :: U [(Float,Float,Float)]) <- createProgram [vs,fs] (\f -> f $ Left "colors")
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer [anySPBatch . shaderProgramBatch program colorsU colors $ [stdRenderCmd mempty () triangle]]
    endFrame window

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
