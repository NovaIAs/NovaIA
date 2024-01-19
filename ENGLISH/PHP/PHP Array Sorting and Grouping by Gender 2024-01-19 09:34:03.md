```php
<?php

// Define the input data
$data = array(
    array(
        'name' => 'John Doe',
        'age' => 30,
        'gender' => 'male'
    ),
    array(
        'name' => 'Jane Smith',
        'age' => 25,
        'gender' => 'female'
    ),
    array(
        'name' => 'Michael Jones',
        'age' => 40,
        'gender' => 'male'
    ),
    array(
        'name' => 'Mary Johnson',
        'age' => 35,
        'gender' => 'female'
    )
);

// Define the function to sort the data by age
function sortByAge($a, $b) {
    return $a['age'] - $b['age'];
}

// Sort the data by age
usort($data, 'sortByAge');

// Define the function to group the data by gender
function groupByGender($data) {
    $groups = array();
    foreach ($data as $row) {
        $gender = $row['gender'];
        if (!isset($groups[$gender])) {
            $groups[$gender] = array();
        }
        $groups[$gender][] = $row;
    }
    return $groups;
}

// Group the data by gender
$groups = groupByGender($data);

// Print the data
foreach ($groups as $gender => $group) {
    echo "<h2>$gender</h2>";
    echo "<ul>";
    foreach ($group as $row) {
        echo "<li>{$row['name']} ({$row['age']})</li>";
    }
    echo "</ul>";
}

?>
```

Explanation:

1. Define the input data (`$data`) as an array of associative arrays, each representing a person with fields for `name`, `age`, and `gender`.

2. Define the `sortByAge` function, which compares two arrays based on the `age` field and returns the difference between them. This function is used to sort the data by age.

3. Sort the data by age using `usort` and the `sortByAge` function. This will arrange the data in ascending order based on the age of each person.

4. Define the `groupByGender` function, which takes an array of data as input and groups it by gender. It creates an associative array where the keys are the genders and the values are arrays of people belonging to that gender.

5. Group the data by gender using the `groupByGender` function. This will create a new array with two keys: "male" and "female", and each key will have an array of people of that gender.

6. Loop through the groups and print the data for each gender. For each gender, print a heading with the gender and then a list of people belonging to that gender, including their name and age.