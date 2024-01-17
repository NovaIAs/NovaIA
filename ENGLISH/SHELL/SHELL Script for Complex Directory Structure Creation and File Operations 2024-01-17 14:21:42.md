```
#!/bin/bash

# This script creates a complex directory structure with nested subdirectories and files, and then performs various operations on them.

# Define the main directory where the structure will be created.
main_dir="complex_structure"

# Define the subdirectories and files to be created within the main directory.
subdirs=("sub1" "sub2" "sub3")
files=("file1.txt" "file2.txt" "file3.txt")

# Create the main directory.
mkdir $main_dir

# Create the subdirectories within the main directory.
for subdir in "${subdirs[@]}"; do
  mkdir "$main_dir/$subdir"
done

# Create the files within the subdirectories.
for file in "${files[@]}"; do
  touch "$main_dir/$subdir/$file"
done

# Display the contents of each subdirectory.
for subdir in "${subdirs[@]}"; do
  echo "Contents of $main_dir/$subdir:"
  ls "$main_dir/$subdir"
  echo
done

# Find all files with a specific extension within the main directory and its subdirectories.
extension=".txt"
find "$main_dir" -name "*$extension"

# Compress all files with a specific extension within the main directory and its subdirectories into a single archive file.
tar -czf "$main_dir.tar.gz" "$main_dir"

# Delete all files with a specific extension within the main directory and its subdirectories.
rm -rf "$main_dir"/*"$extension"

# Delete the main directory and all its contents.
rm -rf "$main_dir"

```

**Explanation:**

1. **Create the Main Directory:**

   - Use the `mkdir` command to create the main directory specified by the `main_dir` variable.

2. **Create Subdirectories:**

   - Use a `for` loop to iterate through the `subdirs` array.
   - For each subdirectory name in the array, use `mkdir` to create a subdirectory within the main directory.

3. **Create Files:**

   - Use another `for` loop to iterate through the `files` array.
   - For each file name in the array, use `touch` to create a file within the subdirectories.

4. **Display Subdirectory Contents:**

   - Use another `for` loop to iterate through the `subdirs` array again.
   - For each subdirectory, use `ls` to list the contents of that subdirectory and echo the result.

5. **Find Files with Specific Extension:**

   - Use the `find` command to search for all files with the specified `extension` within the main directory and its subdirectories.

6. **Compress Files into Archive:**

   - Use the `tar` command to compress all files with the specified `extension` into a single archive file with the `.tar.gz` extension.

7. **Delete Files with Specific Extension:**

   - Use the `rm` command with the `-rf` option to recursively delete all files with the specified `extension` within the main directory and its subdirectories.

8. **Delete Main Directory:**

   - Use the `rm` command with the `-rf` option to recursively delete the main directory and all its contents.