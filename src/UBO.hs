import Common
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Luminance
import Graphics.UI.GLFW
import Linear

main :: IO ()
main = startup $ \window -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  colorBuffer :: Region RW (UB (V3 Float)) <- createBuffer (newRegion 3)
  writeWhole colorBuffer (map UB colors)
  (program,colorsU) <- createProgram [vs,fs] $ \_ uniBlock -> uniBlock "Colors"
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer [anySPBatch $ shaderProgramBatch program colorsU colorBuffer [stdRenderCmd_ triangle]]
    endFrame window

colors :: [V3 Float]
colors = [V3 1 0 0,V3 0 1 0,V3 0 0 1]

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

  , "uniform Colors {"
  , "  vec3 colors[3];"
  , "};"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "  vertexColor = vec4(colors[gl_VertexID], 1.);"
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
