import Common
import Graphics.Luminance

main :: IO ()
main = startup $ \window loop -> do
  triangle <- createGeometry vertices Nothing Triangle
  program <- sequenceA [createStage VertexShader vsSource,createStage FragmentShader fsSource] >>= createProgram_
  loop $ do
    gpuRegion . newFrame defaultFramebuffer . newShading (Some program) $
      drawGeometry (stdRenderCmd triangle)
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

