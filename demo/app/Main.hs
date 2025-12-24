{-# LANGUAGE RecordWildCards #-}

module Main where

import Demo.Backend
import qualified Demo.Mandelbrot as Mandelbrot
import qualified Demo.NBody as NBody
import qualified Demo.Image as Image

import qualified Codec.Picture as JP
import Options.Applicative
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.Array.Accelerate as A

-- | Command line options
data Options = Options
  { optCommand :: Command
  , optBackend :: BackendChoice
  }

data Command
  = Mandelbrot MandelbrotOpts
  | NBodySim NBodyOpts
  | ImageProc ImageOpts

data MandelbrotOpts = MandelbrotOpts
  { mandWidth   :: Int
  , mandHeight  :: Int
  , mandIters   :: Int
  , mandOutput  :: FilePath
  , mandXMin    :: Float
  , mandXMax    :: Float
  , mandYMin    :: Float
  , mandYMax    :: Float
  }

data NBodyOpts = NBodyOpts
  { nbodyParticles :: Int
  , nbodyFrames    :: Int
  , nbodyOutput    :: FilePath
  , nbodyImgWidth  :: Int
  , nbodyImgHeight :: Int
  }

data ImageOpts = ImageOpts
  { imgInput  :: FilePath
  , imgOutput :: FilePath
  , imgMode   :: String  -- "blur" or "edges"
  }

-- | Parse command line options
parseOptions :: Parser Options
parseOptions = Options
  <$> parseCommand
  <*> parseBackend

parseBackend :: Parser BackendChoice
parseBackend = option readBackend
  ( long "backend"
  <> short 'b'
  <> value Auto
  <> help "Backend selection: auto, cpu"
  <> metavar "BACKEND"
  )
  where
    readBackend = maybeReader $ \s -> case s of
      "auto" -> Just Auto
      "cpu"  -> Just CPU
      _      -> Nothing

parseCommand :: Parser Command
parseCommand = subparser
  ( command "mandelbrot" (info parseMandelbrot (progDesc "Generate Mandelbrot set"))
  <> command "nbody"      (info parseNBody (progDesc "Run N-body simulation"))
  <> command "imgproc"    (info parseImage (progDesc "Image processing pipeline"))
  )

parseMandelbrot :: Parser Command
parseMandelbrot = Mandelbrot <$> (MandelbrotOpts
  <$> option auto
      ( long "width"
      <> short 'w'
      <> value 1024
      <> help "Image width"
      <> metavar "INT"
      )
  <*> option auto
      ( long "height"
      <> short 'h'
      <> value 768
      <> help "Image height"
      <> metavar "INT"
      )
  <*> option auto
      ( long "iterations"
      <> short 'i'
      <> value 255
      <> help "Maximum iterations"
      <> metavar "INT"
      )
  <*> strOption
      ( long "output"
      <> short 'o'
      <> value "docs/mandelbrot.png"
      <> help "Output file path"
      <> metavar "FILE"
      )
  <*> option auto
      ( long "xmin"
      <> value (-2.5)
      <> help "View x minimum"
      )
  <*> option auto
      ( long "xmax"
      <> value 1.0
      <> help "View x maximum"
      )
  <*> option auto
      ( long "ymin"
      <> value (-1.0)
      <> help "View y minimum"
      )
  <*> option auto
      ( long "ymax"
      <> value 1.0
      <> help "View y maximum"
      )
  )

parseNBody :: Parser Command
parseNBody = NBodySim <$> (NBodyOpts
  <$> option auto
      ( long "particles"
      <> short 'n'
      <> value 100
      <> help "Number of particles"
      <> metavar "INT"
      )
  <*> option auto
      ( long "frames"
      <> short 'f'
      <> value 60
      <> help "Number of frames to generate"
      <> metavar "INT"
      )
  <*> strOption
      ( long "output"
      <> short 'o'
      <> value "docs/nbody"
      <> help "Output directory for frames"
      <> metavar "DIR"
      )
  <*> option auto
      ( long "width"
      <> short 'w'
      <> value 800
      <> help "Frame width"
      )
  <*> option auto
      ( long "height"
      <> short 'h'
      <> value 600
      <> help "Frame height"
      )
  )

parseImage :: Parser Command
parseImage = ImageProc <$> (ImageOpts
  <$> strOption
      ( long "input"
      <> short 'i'
      <> help "Input image file"
      <> metavar "FILE"
      )
  <*> strOption
      ( long "output"
      <> short 'o'
      <> help "Output image file"
      <> metavar "FILE"
      )
  <*> strOption
      ( long "mode"
      <> short 'm'
      <> value "edges"
      <> help "Processing mode: blur, edges"
      <> metavar "MODE"
      )
  )

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper)
    ( fullDesc
    <> progDesc "Accelerate LLVM Demo - Showcase for CPU and GPU backends"
    <> header "accelerate-llvm-demo - high-performance parallel computing demos"
    )
  
  backend <- selectBackend (optBackend opts)
  
  case optCommand opts of
    Mandelbrot mopts -> runMandelbrot backend mopts
    NBodySim nopts   -> runNBody backend nopts
    ImageProc iopts  -> runImageProc backend iopts

-- | Run Mandelbrot generation
runMandelbrot :: Backend -> MandelbrotOpts -> IO ()
runMandelbrot backend MandelbrotOpts{..} = do
  putStrLn $ "Generating Mandelbrot set: " ++ show mandWidth ++ "x" ++ show mandHeight
  
  let computation = Mandelbrot.mandelbrotImage
        mandWidth mandHeight mandIters
        mandXMin mandXMax mandYMin mandYMax
  
  result <- runWithBackend backend computation
  
  let image = Mandelbrot.renderMandelbrot mandIters result
  
  createDirectoryIfMissing True (takeDirectory mandOutput)
  JP.savePngImage mandOutput (JP.ImageRGB8 image)
  
  putStrLn $ "Saved Mandelbrot image to: " ++ mandOutput
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

-- | Run N-body simulation
runNBody :: Backend -> NBodyOpts -> IO ()
runNBody backend NBodyOpts{..} = do
  putStrLn $ "Running N-body simulation: " ++ show nbodyParticles ++ " particles, " ++ show nbodyFrames ++ " frames"
  
  let initialBodies = NBody.initRandomBodies nbodyParticles
  
  createDirectoryIfMissing True nbodyOutput
  
  -- Simulate and save frames
  let simulate _ bodies 0 = return ()
      simulate frameNum bodies remaining = do
        let computation = A.use bodies A.>-> NBody.nBodyStep
        bodies' <- runWithBackend backend computation
        
        let image = NBody.renderBodiesFrame nbodyImgWidth nbodyImgHeight bodies'
            framePath = nbodyOutput </> "frame_" ++ pad4 frameNum ++ ".png"
        
        JP.savePngImage framePath (JP.ImageRGB8 image)
        
        when (frameNum `mod` 10 == 0) $
          putStrLn $ "Generated frame " ++ show frameNum ++ "/" ++ show nbodyFrames
        
        simulate (frameNum + 1) bodies' (remaining - 1)
  
  simulate 0 initialBodies nbodyFrames
  putStrLn $ "Saved " ++ show nbodyFrames ++ " frames to: " ++ nbodyOutput
  where
    pad4 n = replicate (4 - length s) '0' ++ s
      where s = show n
    when True action = action
    when False _ = return ()

-- | Run image processing
runImageProc :: Backend -> ImageOpts -> IO ()
runImageProc backend ImageOpts{..} = do
  putStrLn $ "Processing image: " ++ imgInput ++ " -> " ++ imgOutput
  
  -- Load image
  eimg <- JP.readImage imgInput
  case eimg of
    Left err -> putStrLn $ "Error loading image: " ++ err
    Right dynImg -> do
      let img = JP.convertRGB8 dynImg
          inputArray = Image.loadImageAsArray img
          
          computation = case imgMode of
            "blur"  -> Image.blurImage (A.use inputArray)
            "edges" -> Image.sobelEdges (A.use inputArray)
            _       -> Image.sobelEdges (A.use inputArray)
      
      result <- runWithBackend backend computation
      
      let outputImg = Image.saveArrayAsImage result
      
      createDirectoryIfMissing True (takeDirectory imgOutput)
      JP.savePngImage imgOutput (JP.ImageRGB8 outputImg)
      
      putStrLn $ "Saved processed image to: " ++ imgOutput
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse
