import Common
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Contravariant.Divisible
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
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (program,colorOffsetU) <- createProgram [vs,fs] $ \uni -> do
    colorU <- uni $ Left "color"
    offsetU <- uni $ Left "offset"
    pure $ divided colorU offsetU
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer
      [anySPBatch . shaderProgramBatch_ program $
        [
          renderCmd Nothing True colorOffsetU (color0,offset0) triangle 
        , renderCmd Nothing True colorOffsetU (color1,offset1) triangle 
        , renderCmd Nothing True colorOffsetU (color2,offset2) triangle 
        ]
      ]
    endFrame window

color0,color1,color2 :: (Float,Float,Float)
color0 = (1,0,0)
color1 = (0,1,0)
color2 = (0,0,1)

offset0,offset1,offset2 :: (Float,Float)
offset0 = (-0.25,0)
offset1 = (0.25,0)
offset2 = (0,0.25)

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
