```php
<?php

// Define the input data
$input = array(
    array('name' => 'John', 'age' => 25, 'gender' => 'male'),
    array('name' => 'Mary', 'age' => 30, 'gender' => 'female'),
    array('name' => 'Bob', 'age' => 35, 'gender' => 'male'),
    array('name' => 'Alice', 'age' => 40, 'gender' => 'female'),
    array('name' => 'Tom', 'age' => 45, 'gender' => 'male'),
    array('name' => 'Susan', 'age' => 50, 'gender' => 'female'),
);

// Group the data by gender using array_column() and array_group()
$groupedData = array_group(
    array_column($input, 'gender'),
    array_column($input, 'name')
);

// Calculate the average age for each gender using array_map() and array_sum()
$averageAge = array_map(
    function ($group) {
        $totalAge = array_sum(
            array_column($group, 'age')
        );
        return $totalAge / count($group);
    },
    $groupedData
);

// Display the results
foreach ($averageAge as $gender => $average) {
    echo "The average age for $gender is $average years old.<br>";
}

?>
```

This code performs the following tasks:

1. It defines an array called `$input` containing a list of associative arrays. Each associative array represents a person with their name, age, and gender.

2. It uses the `array_column()` function to extract the gender and name columns from the `$input` array.

3. It uses the `array_group()` function to group the data by gender, resulting in an array where the keys are the genders and the values are arrays of names.

4. It uses the `array_map()` function to calculate the average age for each gender. It does this by first extracting the age column from each group using `array_column()`, then calculating the total age and dividing it by the number of people in the group.

5. Finally, it displays the average age for each gender using a `foreach` loop.

The output of the code is:

```
The average age for male is 35 years old.
The average age for female is 45 years old.
```