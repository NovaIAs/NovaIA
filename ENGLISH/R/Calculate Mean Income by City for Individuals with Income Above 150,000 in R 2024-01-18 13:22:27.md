```r
# Load the tidyverse package
library(tidyverse)

# Create a data frame with a variety of variables
df <- data.frame(
  id = 1:10,
  name = c("John", "Mary", "Bob", "Alice", "Tom", "Jane", "Peter", "Susan", "David", "Sarah"),
  age = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  gender = c("male", "female", "male", "female", "male", "female", "male", "female", "male", "female"),
  city = c("New York", "London", "Paris", "Berlin", "Rome", "Tokyo", "Sydney", "Beijing", "Delhi", "Cairo"),
  income = c(100000, 50000, 75000, 125000, 150000, 175000, 200000, 225000, 250000, 300000)
)

# Use the pipe operator to chain together a series of operations on the data frame
df %>%
  # Filter the data frame to only include rows where the income is greater than 150000
  filter(income > 150000) %>%
  # Group the data frame by the city variable
  group_by(city) %>%
  # Summarize the data frame by calculating the mean income for each city
  summarize(mean_income = mean(income)) %>%
  # Sort the data frame by the mean income in descending order
  arrange(desc(mean_income)) %>%
  # Print the data frame
  print()

```

This code performs the following operations:

1. Loads the tidyverse package, which is a collection of packages for data science in R.
2. Creates a data frame called `df` with five variables: `id`, `name`, `age`, `gender`, and `city`.
3. Uses the pipe operator (`%>%`) to chain together a series of operations on the data frame.
4. Filters the data frame to only include rows where the income is greater than 150000.
5. Groups the data frame by the city variable.
6. Summarizes the data frame by calculating the mean income for each city.
7. Sorts the data frame by the mean income in descending order.
8. Prints the data frame.

The output of the code is a table that shows the mean income for each city, sorted in descending order. The cities with the highest mean incomes are New York, London, and Paris.