import Common
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics ( Generic )
import Graphics.Luminance
import Graphics.UI.GLFW
import Linear

main :: IO ()
main = startup $ \window -> do
  triangle <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  colorBuffer :: Region RW (UB Color) <- createBuffer (newRegion 3)
  writeWhole colorBuffer (map UB colors)
  (program,colorsU) <- createProgram [vs,fs] $ \_ uniBlock -> uniBlock "Colors"
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer [anySPBatch $ shaderProgramBatch program colorsU colorBuffer [stdRenderCmd_ triangle]]
    endFrame window

data Color = Color {
    colorsRGB :: V3 Float
  , colorsK   :: Float
  } deriving (Eq,Generic,Show)

instance UniformBlock Color where

colors :: [Color]
colors =
  [
    Color {
      colorsRGB = V3 1 0 0
    , colorsK = 0.75
    }
  , Color {
      colorsRGB = V3 0 1 0
    , colorsK = 0.3
    }
  , Color {
      colorsRGB = V3 0 0 1
    , colorsK = 0.5
    }
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
  , "out vec4 vertexColor;"

  , "struct Color {"
  , "  vec3 colorsRGB;"
  , "  float colorsK;"
  , "};"

  , "layout (std140) uniform Colors {"
  , "  Color colors[3];"
  , "};"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "  vertexColor = vec4(colors[gl_VertexID].colorsRGB * colors[gl_VertexID].colorsK, 1.);"
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
