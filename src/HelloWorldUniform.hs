import Common
import Graphics.Luminance

main :: IO ()
main = startup $ \window loop -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  program <- createProgram [vs,fs] $ \uni ->
    uni (UniformName "colors")
  updateUniforms program (.= colors)
  loop $ do
    _ <- draw $ defaultFrameCmd [ShadingCmd program mempty [pureDraw $ stdRenderCmd triangle]]
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
