```shell
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Define functions
function print_error() {
  echo -e "${RED}$1${NC}"
}

function print_success() {
  echo -e "${GREEN}$1${NC}"
}

function print_warning() {
  echo -e "${YELLOW}$1${NC}"
}

function print_info() {
  echo -e "${BLUE}$1${NC}"
}

function print_debug() {
  echo -e "${MAGENTA}$1${NC}"
}

function check_root() {
  if [[ $EUID -ne 0 ]]; then
    print_error "This script must be run as root."
    exit 1
  fi
}

function install_dependencies() {
  print_info "Installing dependencies..."
  apt-get update
  apt-get install -y build-essential cmake git libgtk2.0-dev libglib2.0-dev \
    libcairo2-dev libpango1.0-dev libjpeg-dev libpng-dev libtiff-dev \
    libavcodec-dev libavformat-dev libswscale-dev libavutil-dev libavresample-dev \
    libx264-dev libx265-dev libmp3lame-dev libopus-dev libvorbis-dev libfdk-aac-dev \
    libvpx-dev libxvidcore-dev libx263-dev

  if [[ $? -ne 0 ]]; then
    print_error "Failed to install dependencies."
    exit 1
  fi

  print_success "Dependencies installed."
}

function clone_ffmpeg() {
  print_info "Cloning FFmpeg repository..."
  git clone https://github.com/FFmpeg/FFmpeg.git

  if [[ $? -ne 0 ]]; then
    print_error "Failed to clone FFmpeg repository."
    exit 1
  fi

  print_success "FFmpeg repository cloned."
}

function build_ffmpeg() {
  print_info "Building FFmpeg..."
  cd FFmpeg

  ./configure \
    --prefix=/usr/local \
    --disable-ffplay \
    --disable-ffprobe \
    --disable-doc \
    --disable-debug \
    --enable-gpl \
    --enable-libass \
    --enable-libfdk-aac \
    --enable-libfreetype \
    --enable-libmp3lame \
    --enable-libopus \
    --enable-libvorbis \
    --enable-libvpx \
    --enable-libx264 \
    --enable-libx265 \
    --enable-libxvid

  if [[ $? -ne 0 ]]; then
    print_error "Failed to configure FFmpeg."
    exit 1
  fi

  make -j$(nproc)

  if [[ $? -ne 0 ]]; then
    print_error "Failed to build FFmpeg."
    exit 1
  fi

  make install

  if [[ $? -ne 0 ]]; then
    print_error "Failed to install FFmpeg."
    exit 1
  fi

  cd ..

  print_success "FFmpeg built and installed."
}

function cleanup() {
  print_info "Cleaning up..."
  rm -rf FFmpeg

  print_success "Cleanup complete."
}

# Check if script is being run as root
check_root

# Install dependencies
install_dependencies

# Clone FFmpeg repository
clone_ffmpeg

# Build FFmpeg
build_ffmpeg

# Cleanup
cleanup
```

This code is a shell script that installs FFmpeg on a Linux system. It includes functions for printing messages in different colors, checking if the script is being run as root, installing dependencies, cloning the FFmpeg repository, building FFmpeg, and cleaning up.

The script first checks if the user is running the script as root. If the user is not root, the script prints an error message and exits.

Next, the script installs the dependencies required to build FFmpeg. These dependencies include build-essential, cmake, git, and various development libraries.

After the dependencies are installed, the script clones the FFmpeg repository from GitHub.

Once the FFmpeg repository is cloned, the script configures FFmpeg with various options. These options include disabling certain features, enabling others, and specifying the installation prefix.

After FFmpeg is configured, the script builds FFmpeg using the make command. The -j$(nproc) option tells make to use all available CPU cores to speed up the build process.

Once FFmpeg is built, the script installs it using the make install command.

Finally, the script cleans up by removing the FFmpeg repository directory.