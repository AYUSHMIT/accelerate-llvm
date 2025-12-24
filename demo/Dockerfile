# Accelerate LLVM Demo - CPU-only Docker image
FROM haskell:9.6-slim as builder

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    clang \
    libffi-dev \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy source files
COPY cabal.project ./
COPY LICENSE ./
COPY accelerate-llvm ./accelerate-llvm
COPY accelerate-llvm-native ./accelerate-llvm-native
COPY demo ./demo

# Initialize submodules if needed
RUN git init && \
    git config --global user.email "docker@example.com" && \
    git config --global user.name "Docker Build" && \
    (git submodule update --init --recursive || true)

# Build the demo
RUN cabal update && \
    cabal build accelerate-llvm-demo --enable-executable-static

# Find the built executable
RUN find dist-newstyle -name accelerate-llvm-demo -type f -executable -exec cp {} /build/demo-exe \;

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y \
    clang \
    libffi8 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy executable from builder
COPY --from=builder /build/demo-exe /usr/local/bin/accelerate-llvm-demo

# Create output directory
RUN mkdir -p /app/docs

# Default command: generate a Mandelbrot set
CMD ["accelerate-llvm-demo", "mandelbrot", \
     "--width", "1024", \
     "--height", "768", \
     "--output", "/app/docs/mandelbrot.png", \
     "--backend", "cpu"]
