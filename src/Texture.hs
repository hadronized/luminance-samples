import Codec.Picture
import Codec.Picture.Types
import Control.Concurrent
import Control.Monad
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.Resource
import Data.Foldable ( for_ )
import Data.Vector ( Vector, convert )
import qualified Data.Vector.Storable as S ( Vector )
import Foreign.Storable ( Storable )
import Graphics.Luminance.Batch
import Graphics.Luminance.Framebuffer
import Graphics.Luminance.Geometry
import Graphics.Luminance.RenderCmd
import Graphics.Luminance.Pixel as L
import Graphics.Luminance.Shader.Program
import Graphics.Luminance.Shader.Stage
import Graphics.Luminance.Texture
import Graphics.Luminance.Vertex
import Graphics.UI.GLFW
import Prelude hiding ( init )
import System.Environment ( getArgs )

data AppError = AppError String deriving (Eq,Show)

instance HasFramebufferError AppError where
  fromFramebufferError e = AppError (show e)

instance HasStageError AppError where
  fromStageError e = AppError (show e)

instance HasProgramError AppError where
  fromProgramError e = AppError (show e)

windowW,windowH :: (Num a) => a
windowW = 800
windowH = 600

windowTitle :: String
windowTitle = "Texture"

main :: IO ()
main = do
  _ <- init
  windowHint (WindowHint'Resizable False)
  windowHint (WindowHint'ContextVersionMajor 4)
  windowHint (WindowHint'ContextVersionMinor 5)
  windowHint (WindowHint'OpenGLForwardCompat False)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  window <- createWindow windowW windowH windowTitle Nothing Nothing
  makeContextCurrent window
  for_ window $ \window' -> do
    (runResourceT . runExceptT . app) window' >>= either print (const $ pure ())
    destroyWindow window'
  terminate

app :: (MonadError AppError m,MonadIO m,MonadResource m) => Window -> m ()
app window = do
  args <- liftIO getArgs
  when (null args) . throwError $ AppError "expecting a texture path as argument!"
  tex <- loadTexture (head args)
  quad <- createGeometry vertices Nothing Triangle
  vs <- createVertexShader vsSource
  fs <- createFragmentShader fsSource
  (program,texU) <- createProgram [vs,fs] $ \uni -> do
    uni $ Left "srcTex"
  untilM (liftIO $ windowShouldClose window) $ do
    treatFBBatch $ framebufferBatch defaultFramebuffer
      [anySPBatch $ SPBatch program texU tex [renderCmd Nothing True mempty () quad]]
    liftIO $ do
      pollEvents
      swapBuffers window
      threadDelay 50000

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
    "#version 450 core"
  
  , "in vec2 co;"

  , "void main() {"
  , "  gl_Position = vec4(co, 0., 1.);"
  , "}"
  ]

fsSource :: String
fsSource = unlines
  [
    "#version 450 core"
  , "#extension GL_ARB_bindless_texture : require"

  , "out vec4 frag;"

  , "layout (bindless_sampler) uniform usampler2D srcTex;"

  , "void main() {"
  , "  vec2 uv = gl_FragCoord.xy;"
  , "  uv.y = " ++ show windowH ++ " - uv.y;"
  , "  vec3 sampled = vec3(texture(srcTex, uv / vec2(" ++ show windowW ++ "," ++ show windowH ++ "), 0).rgb) / 255;"
  , "  frag = vec4(sampled, 1.);"
  , "}"
  ]

untilM :: (Monad m) => m Bool -> m b -> m ()
untilM predicate a = go
  where
    go = do
      p <- predicate
      if p then pure () else a >> go

loadTexture :: (MonadError AppError m,MonadIO m,MonadResource m) => String -> m (Texture2D RGBA8UI)
loadTexture path = liftIO (readImage path) >>= either (throwError . AppError) treatDynamicImage
  where
    treatDynamicImage image = case image of
      ImageRGBA8 img  -> liftIO (putStrLn "ImageRGBA8") >> createTexture_ img
      ImageYCbCr8 img ->  liftIO (putStrLn "ImageYCbCr8") >> createTexture_ (promoteImage $ (convertImage img :: Image PixelRGB8))
      _ -> throwError $ AppError "unsupported image format"

createTexture_ :: (MonadIO m,MonadResource m) => Image PixelRGBA8 -> m (Texture2D RGBA8UI)
createTexture_ img = do
  tex <- createTexture (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img) 1 defaultSampling
  uploadWhole tex False (convertV $ imageData img)
  pure tex

convertV :: (Storable a) => S.Vector a -> Vector a
convertV = convert
