import Common
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Luminance
import Graphics.UI.GLFW

main :: IO ()
main = startup $ \window mainLoop -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  (program,colorsU :: U [(Float,Float,Float)]) <- createProgram [vs,fs] $ \uni _ ->
    uni (Left "colors")
  mainLoop $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer [anySPBatch $ shaderProgramBatch program colorsU colors [stdRenderCmd_ triangle]]
    endFrame window

colors :: [(Float,Float,Float)]
colors = [(1,0,0),(0,1,0),(0,0,1)]

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
    "in vec4 vertexColor;"
  , "out vec4 frag;"


  , "void main() {"
  , "  frag = vertexColor;"
  , "}"
  ]
