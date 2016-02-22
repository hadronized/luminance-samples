import Common
import Data.Foldable ( for_ )
import Graphics.Luminance

main :: IO ()
main = startup $ \window loop -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  program <- createProgram [vs,fs] $ \uni -> do
    colorU <- uni (UniformName "color")
    offsetU <- uni (UniformName "offset")
    pure (colorU,offsetU)
  loop $ do
    gpuRegion . newFrame defaultFramebuffer . newShading program $ \updateUniforms -> do
      for_ (zip colors offsets) $ \(color,offset) -> do
        updateUniforms $ \(colorU,offsetU) ->
             colorU .= color
          <> offsetU .= offset
        drawGeometry (renderCmd Nothing True triangle )
    endFrame window

colors :: [(Float,Float,Float)]
colors =
  [
    (1,0,0)
  , (0,1,0)
  , (0,0,1)
  ]

offsets :: [(Float,Float)]
offsets =
  [
    (-0.25,0)
  , (0.25,0)
  , (0,0.25)
  ]

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

  , "uniform vec2 offset;"

  , "void main() {"
  , "  gl_Position = vec4(co + offset, 0., 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "out vec4 frag;"

  , "uniform vec3 color;"

  , "void main() {"
  , "  frag = vec4(color, 1.);"
  , "}"
  ]
