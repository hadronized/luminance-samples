import Codec.Picture
import Codec.Picture.Types
import Common
import Control.Monad
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Graphics.Luminance
import Graphics.UI.GLFW
import System.Environment ( getArgs )

main :: IO ()
main = startup $ \window mainLoop -> do
  args <- liftIO getArgs
  when (null args) . throwError $ CLIUsage "expecting a texture path as argument!"
  tex <- loadTexture (head args)
  quad <- createGeometry vertices Nothing Triangle
  vs <- createStage VertexShader vsSource
  fs <- createStage FragmentShader fsSource
  (program,texU) <- createProgram [vs,fs] $ \uni _ -> uni $ Left "srcTex"
  mainLoop $ do
    void . runCmd . draw $ framebufferBatch defaultFramebuffer
      [anySPBatch $ shaderProgramBatch program texU tex [stdRenderCmd_ quad]]
    endFrame window

vertices :: [V 2 Float]
vertices =
  [
    vec2 (-1) 1
  , vec2 1 1
  , vec2 (-1) (-1)
  , vec2 (-1) (-1)
  , vec2 1 1
  , vec2 1 (-1)
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

  , "uniform sampler2D srcTex;"

  , "void main() {"
  , "  vec2 uv = gl_FragCoord.xy;"
  , "  uv.y = " ++ show (windowH :: Int) ++ " - uv.y;"
  , "  frag = texture(srcTex, uv / vec2(" ++ show (windowW :: Int) ++ "," ++ show (windowH :: Int) ++ "), 0);"
  , "}"
  ]

loadTexture :: (MonadError AppError m,MonadIO m,MonadResource m) => String -> m (Texture2D RGB32F)
loadTexture path = liftIO (readImage path) >>= either (throwError . TextureLoadFailed) treatDynamicImage
  where
    treatDynamicImage image = case image of
      ImageRGBA8 img  -> createTexture_ (promoteImage $ dropAlphaLayer img)
      ImageYCbCr8 img -> createTexture_ (promoteImage (convertImage img :: Image PixelRGB8))
      _ -> throwError (TextureLoadFailed "unsupported image format")

createTexture_ :: (MonadIO m,MonadResource m) => Image PixelRGBF -> m (Texture2D RGB32F)
createTexture_ img = do
  let w = fromIntegral $ imageWidth img
      h = fromIntegral $ imageHeight img
  tex <- createTexture (w,h) 1 defaultSampling
  uploadSub tex (0,0) (w,h) False (imageData img)
  pure tex
