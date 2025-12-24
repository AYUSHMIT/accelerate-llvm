#!/usr/bin/env bash
# Script to run benchmarks and generate outputs

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEMO_DIR="$(dirname "$SCRIPT_DIR")"
DOCS_DIR="$DEMO_DIR/docs"

echo "=== Accelerate LLVM Demo Benchmark Runner ==="
echo ""

# Ensure output directory exists
mkdir -p "$DOCS_DIR"

# Build the demo
echo "Building demo..."
cabal build accelerate-llvm-demo

echo ""
echo "=== Running Mandelbrot demos ==="

# Generate standard Mandelbrot
echo "Generating standard Mandelbrot (1024x768)..."
cabal run accelerate-llvm-demo -- mandelbrot \
  --width 1024 --height 768 \
  --iterations 255 \
  --output "$DOCS_DIR/mandelbrot.png" \
  --backend auto

# Generate zoomed view
echo "Generating zoomed Mandelbrot..."
cabal run accelerate-llvm-demo -- mandelbrot \
  --width 1024 --height 768 \
  --iterations 512 \
  --xmin -0.5 --xmax -0.4 \
  --ymin 0.5 --ymax 0.6 \
  --output "$DOCS_DIR/mandelbrot_zoom.png" \
  --backend auto

echo ""
echo "=== Running N-body simulation ==="

# Generate N-body frames
echo "Simulating N-body (100 particles, 30 frames)..."
mkdir -p "$DOCS_DIR/nbody"
cabal run accelerate-llvm-demo -- nbody \
  --particles 100 \
  --frames 30 \
  --width 800 --height 600 \
  --output "$DOCS_DIR/nbody" \
  --backend auto

echo ""
echo "=== Running benchmarks ==="

# Run Criterion benchmarks
cabal run demo-bench -- --output "$DOCS_DIR/benchmark.html"

echo ""
echo "=== Done ==="
echo "Outputs saved to: $DOCS_DIR"
echo "  - mandelbrot.png"
echo "  - mandelbrot_zoom.png"
echo "  - nbody/*.png (frame sequence)"
echo "  - benchmark.html"
