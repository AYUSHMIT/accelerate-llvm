# Accelerate LLVM Demo

This demo showcases the Accelerate array language with LLVM backends for high-performance parallel computing on CPUs and (optionally) GPUs.

## Features

- **Mandelbrot Set Renderer**: Generate beautiful fractal images with configurable resolution and zoom levels
- **N-body Simulation**: Physics-based particle simulation with frame sequence generation
- **Image Processing Pipeline**: Blur and edge detection operations on images

All demos support automatic backend selection with graceful fallback from GPU to CPU.

## Building

### Prerequisites

- GHC >= 9.4
- Cabal >= 3.8
- clang (for LLVM backend)
- libFFI (for CPU backend)
- CUDA toolkit (optional, for GPU backend)

### Build Instructions

```bash
# From repository root
cabal update
cabal build accelerate-llvm-demo

# Or from demo directory
cd demo
cabal build
```

## Usage

### Mandelbrot Set

Generate a Mandelbrot set image:

```bash
cabal run accelerate-llvm-demo -- mandelbrot \
  --width 2048 \
  --height 1536 \
  --iterations 255 \
  --output docs/mandelbrot.png \
  --backend auto
```

Custom view window:

```bash
cabal run accelerate-llvm-demo -- mandelbrot \
  --width 1024 --height 768 \
  --xmin -0.5 --xmax -0.4 \
  --ymin 0.5 --ymax 0.6 \
  --output zoom.png
```

### N-body Simulation

Run an N-body simulation and generate frame sequence:

```bash
cabal run accelerate-llvm-demo -- nbody \
  --particles 200 \
  --frames 120 \
  --width 800 \
  --height 600 \
  --output docs/nbody \
  --backend auto
```

This creates a sequence of PNG frames in the output directory.

### Image Processing

Apply image processing operations:

```bash
# Edge detection
cabal run accelerate-llvm-demo -- imgproc \
  --input assets/sample.png \
  --output docs/edges.png \
  --mode edges

# Blur
cabal run accelerate-llvm-demo -- imgproc \
  --input assets/sample.png \
  --output docs/blurred.png \
  --mode blur
```

## Backend Selection

The demo supports multiple backends:

- `--backend auto`: Automatically select the best available backend (default)
- `--backend cpu`: Force CPU backend (accelerate-llvm-native)

GPU backend support can be added by building with `accelerate-llvm-ptx`.

## Benchmarks

Run performance benchmarks:

```bash
cabal run demo-bench
```

This uses Criterion to benchmark the Mandelbrot and N-body implementations.

## Testing

Run the test suite:

```bash
cabal test demo-tests
```

## Docker

Build and run in a Docker container (CPU-only):

```bash
# From repository root
docker build -t accelerate-demo -f Dockerfile .
docker run accelerate-demo
```

## Output Gallery

Generated images and animations are saved to the `docs/` directory, which is also published to GitHub Pages.

## Performance Notes

- The CPU backend uses all available cores via LLVM's native code generation
- Larger problem sizes show better parallel speedup
- GPU backend (when available) provides significant speedup for compute-intensive workloads

## Contributing

This demo is part of the accelerate-llvm project. For issues and contributions, see the main repository.

## License

BSD-3-Clause (same as parent project)
