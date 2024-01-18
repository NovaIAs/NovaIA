```shell
#!/bin/bash

# Define global variables
source_directory="/path/to/source"
backup_directory="/path/to/backup"
log_file="/path/to/log.txt"

# Function to display usage message
usage() {
    echo "Usage: $0 [-s source_directory] [-b backup_directory] [-l log_file]"
    echo "Options:"
    echo "  -s source_directory: Path to the source directory containing files to be backed up"
    echo "  -b backup_directory: Path to the backup directory where files will be copied"
    echo "  -l log_file: Path to the log file where the script's activities will be recorded"
    exit 1
}

# Parse command line arguments
while getopts "s:b:l:" opt; do
    case "$opt" in
    s)  source_directory="$OPTARG" ;;
    b)  backup_directory="$OPTARG" ;;
    l)  log_file="$OPTARG" ;;
    ?)  usage ;;
    esac
done

# Check if source directory exists
if [ ! -d "$source_directory" ]; then
    echo "Error: Source directory $source_directory does not exist"
    exit 1
fi

# Check if backup directory exists, if not create it
if [ ! -d "$backup_directory" ]; then
    mkdir -p "$backup_directory"
fi

# Start logging
exec > >(tee -a "$log_file") 2>&1

# Iterate over files in the source directory
find "$source_directory" -type f -print0 | while IFS= read -r file; do
    # Get file size
    file_size=$(stat -c %s "$file")

    # Check if file is larger than 1 GB
    if [ "$file_size" -gt 1073741824 ]; then
        # Display message about large file
        echo "Found large file: $file"

        # Create a symlink in the backup directory
        ln -s "$file" "$backup_directory/$(basename "$file")"

        # Compress the original file
        gzip -9 "$file"

        # Log the actions taken
        echo "$(date) - Compressed large file: $file" >> "$log_file"
    else
        # Copy the file to the backup directory
        cp "$file" "$backup_directory"

        # Log the action taken
        echo "$(date) - Copied file: $file" >> "$log_file"
    fi
done

# End logging
exec &> /dev/tty

# Display completion message
echo "Backup process complete. Check $log_file for details."
```

This script takes a source directory, a backup directory, and a log file as input. It then iterates over all the files in the source directory and performs the following actions:

- If the file is larger than 1 GB, it creates a symlink to the file in the backup directory and compresses the original file.
- If the file is smaller than 1 GB, it simply copies the file to the backup directory.
- In both cases, it logs the action taken in the log file.

The script also handles cases where the source directory does not exist or the backup directory does not exist, and it displays a usage message if the input arguments are not provided correctly.