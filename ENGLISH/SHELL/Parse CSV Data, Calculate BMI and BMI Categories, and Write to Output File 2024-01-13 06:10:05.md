```shell
#!/bin/bash

# Set up the variables we'll need
input_file="input.txt"
output_file="output.txt"
temp_file="temp.txt"

# Check if the input file exists
if [ ! -f "$input_file" ]; then
  echo "Error: Input file $input_file does not exist"
  exit 1
fi

# Create a temporary file to store the intermediate results
touch "$temp_file"

# Read the input file line by line and process each line
while read line; do

  # Split the line into fields using a comma as the delimiter
  IFS=',' read -a fields <<< "$line"

  # Check if the first field is "name"
  if [ "${fields[0]}" == "name" ]; then

    # If it is, then this is the header line.
    # Write it to the output file and skip to the next line.
    echo "$line" > "$output_file"
    continue

  fi

  # If the first field is not "name", then this is a data line.
  # Process the data fields.

  # Convert the second field (age) to an integer
  age=${fields[1]}

  # Convert the third field (gender) to lowercase
  gender=${fields[2]}
  gender=$(echo "$gender" | tr '[A-Z]' '[a-z]')

  # Calculate the BMI (body mass index) using the formula:
  # BMI = weight / (height * height)
  # Convert the fourth field (weight) to a floating-point number
  weight=${fields[3]}
  weight=$(echo "$weight" | tr ',' '.')

  # Convert the fifth field (height) to a floating-point number
  height=${fields[4]}
  height=$(echo "$height" | tr ',' '.')

  bmi=$(echo "scale=2; $weight / ($height * $height)" | bc)

  # Check if the BMI is in the healthy range (18.5 to 24.9)
  if (( $(echo "$bmi < 18.5" | bc -l) )); then
    bmi_category="Underweight"
  elif (( $(echo "$bmi < 25" | bc -l) )); then
    bmi_category="Healthy"
  elif (( $(echo "$bmi < 30" | bc -l) )); then
    bmi_category="Overweight"
  else
    bmi_category="Obese"
  fi

  # Write the processed data to the temporary file
  echo "${fields[0]},$age,$gender,$weight,$height,$bmi,$bmi_category" >> "$temp_file"

done < "$input_file"

# Sort the temporary file by name
sort "$temp_file" > "$output_file"

# Remove the temporary file
rm "$temp_file"

echo "Processed data written to $output_file"
```

This shell script takes an input file containing a list of people's names, ages, genders, weights, and heights, and processes the data to calculate their BMIs (body mass indices) and BMI categories. The processed data is then written to an output file.

Here's a detailed explanation of the code:

1. **Input and Output Files:**

   - `input_file`: The input file containing the data to be processed.
   - `output_file`: The output file where the processed data will be written.
   - `temp_file`: A temporary file used to store intermediate results.

2. **Processing the Input File:**

   The script uses a `while` loop to read the input file line by line.

3. **Processing Each Line:**

   For each line, the script splits it into fields using a comma as the delimiter.

4. **Handling the Header Line:**

   The first line of the input file is assumed to be the header line, containing the field names. It is written to the output file as-is and skipped.

5. **Processing Data Lines:**

   - **Age:** The age field is converted to an integer.
   - **Gender:** The gender field is converted to lowercase.
   - **Weight:** The weight field is converted to a floating-point number.
   - **Height:** The height field is converted to a floating-point number.

6. **Calculating BMI:**

   - The BMI is calculated using the formula: `BMI = weight / (height * height)`.
   - The result is rounded to two decimal places.

7. **Determining BMI Category:**

   - The BMI category is determined based on the following ranges:
     - Underweight: BMI < 18.5
     - Healthy: 18.5 <= BMI < 25
     - Overweight: 25 <= BMI < 30
     - Obese: BMI >= 30

8. **Writing Processed Data to Temporary File:**

   - The processed data (name, age, gender, weight, height, BMI, BMI category) is written to the temporary file.

9. **Sorting the Temporary File:**

   - The temporary file is sorted by name.

10. **Moving Sorted Data to Output File:**

    - The sorted data is moved from the temporary file to the output file.

11. **Cleaning Up:**

    - The temporary file is deleted.

12. **Displaying Success Message:**

    - A success message is displayed, indicating that the processed data has been written to the output file.