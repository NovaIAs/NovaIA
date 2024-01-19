```
#!/bin/bash

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Define functions
function check_prerequisites() {
  # Check if necessary programs are installed
  if ! command -v wget &> /dev/null; then
    echo "${RED}Error: wget is not installed.${NC}"
    exit 1
  fi
  if ! command -v tar &> /dev/null; then
    echo "${RED}Error: tar is not installed.${NC}"
    exit 1
  fi
  if ! command -v gzip &> /dev/null; then
    echo "${RED}Error: gzip is not installed.${NC}"
    exit 1
  fi
}

function download_data() {
  # Download the data from the URL
  wget https://www.example.com/data.tar.gz

  # Check if the download was successful
  if [ $? -ne 0 ]; then
    echo "${RED}Error: Unable to download data.${NC}"
    exit 1
  fi
}

function extract_data() {
  # Extract the data from the tarball
  tar -xzvf data.tar.gz

  # Check if the extraction was successful
  if [ $? -ne 0 ]; then
    echo "${RED}Error: Unable to extract data.${NC}"
    exit 1
  fi
}

function clean_up() {
  # Remove the downloaded tarball
  rm data.tar.gz
}

function main() {
  # Check for prerequisites
  check_prerequisites

  # Download the data
  download_data

  # Extract the data
  extract_data

  # Clean up
  clean_up

  # Display success message
  echo "${GREEN}Data downloaded, extracted, and cleaned up successfully.${NC}"
}

# Run the main function
main
```

This script performs the following tasks:

* **Checks for prerequisites:** The script first checks if the necessary programs (wget, tar, and gzip) are installed. If any of these programs are missing, the script displays an error message and exits.

* **Downloads the data:** The script uses wget to download the data file from the specified URL. If the download is successful, the script proceeds to the next step; otherwise, it displays an error message and exits.

* **Extracts the data:** The script uses tar to extract the data from the downloaded tarball. If the extraction is successful, the script proceeds to the next step; otherwise, it displays an error message and exits.

* **Cleans up:** The script removes the downloaded tarball using the rm command.

* **Displays a success message:** If all the previous steps are successful, the script displays a success message.

Here is an explanation of the commands used in the script:

* **wget:** The wget command is used to download files from the internet. In this script, it is used to download the data file.

* **tar:** The tar command is used to compress and extract files. In this script, it is used to extract the data from the downloaded tarball.

* **gzip:** The gzip command is used to compress and decompress files. In this script, it is used to decompress the downloaded tarball.

* **rm:** The rm command is used to remove files. In this script, it is used to remove the downloaded tarball.