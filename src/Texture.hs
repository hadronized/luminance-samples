import Codec.Picture
import Codec.Picture.Types
import Common
import Control.Monad
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Vector ( Vector, convert )
import qualified Data.Vector.Storable as S ( Vector )
import Foreign.Storable ( Storable )
import Graphics.Luminance.Batch
import Graphics.Luminance.Cmd
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.RenderCmd
import Graphics.Luminance.Pixel as L
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Texture
import Graphics.Luminance.Vertex
import Graphics.UI.GLFW
import System.Environment ( getArgs )

main :: IO ()
main = startup $ \window -> do
  args <- liftIO getArgs
  when (null args) . throwError $ AppError "expecting a texture path as argument!"
  tex <- loadTexture (head args)
  quad <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (program,texU) <- createProgram [vs,fs] $ \uni -> do
    uni $ Left "srcTex"
  untilM (liftIO $ windowShouldClose window) $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer
      [anySPBatch $ shaderProgramBatch program texU tex [stdRenderCmd_ quad]]
    endFrame window

vertices :: [V 2 Float]
vertices =
  [
    V2 (-1) 1
  , V2 1 1
  , V2 (-1) (-1)
  , V2 (-1) (-1)
  , V2 1 1
  , V2 1 (-1)
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

  , "layout (bindless_sampler) uniform usampler2D srcTex;"

  , "void main() {"
  , "  vec2 uv = gl_FragCoord.xy;"
  , "  uv.y = " ++ show (windowH :: Int) ++ " - uv.y;"
  , "  vec3 sampled = vec3(texture(srcTex, uv / vec2(" ++ show (windowW :: Int) ++ "," ++ show (windowH :: Int) ++ "), 0).rgb) / 255;"
  , "  frag = vec4(sampled, 1.);"
  , "}"
  ]

loadTexture :: (MonadError AppError m,MonadIO m,MonadResource m) => String -> m (Texture2D RGBA8UI)
loadTexture path = liftIO (readImage path) >>= either (throwError . AppError) treatDynamicImage
  where
    treatDynamicImage image = case image of
      ImageRGBA8 img  -> liftIO (putStrLn "ImageRGBA8") >> createTexture_ img
      ImageYCbCr8 img ->  liftIO (putStrLn "ImageYCbCr8") >> createTexture_ (promoteImage $ (convertImage img :: Image PixelRGB8))
      _ -> throwError $ AppError "unsupported image format"

createTexture_ :: (MonadIO m,MonadResource m) => Image PixelRGBA8 -> m (Texture2D RGBA8UI)
createTexture_ img = do
  let w = fromIntegral $ imageWidth img
      h = fromIntegral $ imageHeight img
  tex <- createTexture (w,h) 1 defaultSampling
  uploadSub tex (0,0) (w,h) False (convertV $ imageData img)
  pure tex

convertV :: (Storable a) => S.Vector a -> Vector a
convertV = convert
