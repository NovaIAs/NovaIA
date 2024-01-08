```portugal
program Find the largest number in an array

integer array numbers[100]
integer i, largest

input "Enter the number of elements in the array: " n
for i = 1 to n do
  input "Enter element ", i, ": " numbers[i]
end for

largest = numbers[1]

for i = 2 to n do
  if numbers[i] > largest then
    largest = numbers[i]
  end if
end for

output "The largest number in the array is: ", largest
```

Explanation:

This program finds the largest number in an array of integers.

The program starts by declaring an array of integers named numbers with a maximum size of 100.

The user is then prompted to enter the number of elements in the array.

A for loop is then used to iterate through the array and input the values of each element.

The largest variable is initialized to the value of the first element in the array.

Another for loop is then used to iterate through the array and compare each element to the largest variable.

If an element is found to be greater than the largest variable, the largest variable is updated to the value of that element.

After the for loop has completed, the largest variable contains the largest number in the array.

The program then outputs the value of the largest variable to the screen.