```
#!/bin/bash

# Specify the input and output directories
input_dir="/path/to/input_directory"
output_dir="/path/to/output_directory"

# List all the files in the input directory
files=( $(ls -1 "$input_dir") )

# Iterate over each file in the input directory
for file in "${files[@]}"; do

  # Get the file extension
  extension="${file##*.}"

  # Convert the file to lowercase
  filename="${file,,}"

  # Check if the file is a text file
  if [[ "$extension" =~ ^(txt|csv|md)$ ]]; then

    # Convert the file to uppercase and move it to the output directory
    tr '[:lower:]' '[:upper:]' < "$input_dir/$file" > "$output_dir/$filename"

  # Check if the file is an image file
  elif [[ "$extension" =~ ^(jpg|png|gif)$ ]]; then

    # Resize the image to a width of 300 pixels and move it to the output directory
    convert "$input_dir/$file" -resize 300x300 "$output_dir/$filename"

  # Check if the file is a video file
  elif [[ "$extension" =~ ^(mp4|mov|avi)$ ]]; then

    # Trim the video to the first 30 seconds and move it to the output directory
    ffmpeg -i "$input_dir/$file" -t 30 -c copy "$output_dir/$filename"

  # Check if the file is an audio file
  elif [[ "$extension" =~ ^(mp3|wav|flac)$ ]]; then

    # Convert the audio file to a different format and move it to the output directory
    ffmpeg -i "$input_dir/$file" -f mp3 -ab 192k "$output_dir/$filename"

  # If the file is not recognized, move it to the output directory as is
  else

    mv "$input_dir/$file" "$output_dir/$filename"

  fi

done

```

Explanation:

1. **Input and Output Directories:** Specify the input directory where the files are located and the output directory where the processed files will be saved.

2. **List Files:** List all the files in the input directory.

3. **Iterate Over Files:** Loop through each file in the input directory.

4. **Get File Extension:** Extract the file extension of the current file.

5. **Convert Filename to Lowercase:** Convert the filename to lowercase for case-insensitive processing.

6. **Check File Type:** Check the file extension to determine the type of file it is.

7. **Process Files Based on Type:**

   - **Text Files:** Convert the text file to uppercase and move it to the output directory.

   - **Image Files:** Resize the image to a width of 300 pixels and move it to the output directory.

   - **Video Files:** Trim the video to the first 30 seconds and move it to the output directory.

   - **Audio Files:** Convert the audio file to a different format and move it to the output directory.

   - **Other Files:** Move the file to the output directory as is.

8. **Move Processed Files:** Move the processed files to the output directory.

This script is designed to process a variety of files based on their type. It can convert text files to uppercase, resize images, trim videos, convert audio files, and move other files as is. The script ensures that the processed files are saved in the specified output directory.