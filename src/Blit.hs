import Common
import Graphics.Luminance

main :: IO ()
main = startup $ \window loop -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  program <- createProgram_ [vs,fs]
  fb :: Framebuffer RW RGBA32F () <- createFramebuffer windowW windowH 1
  loop $ do
    _ <- draw $ FrameCmd fb [ShadingCmd program mempty [pureDraw (stdRenderCmd triangle)]]
    framebufferBlit fb defaultFramebuffer 0 0 windowW windowH 0 0 windowW windowH BlitColor Linear
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
