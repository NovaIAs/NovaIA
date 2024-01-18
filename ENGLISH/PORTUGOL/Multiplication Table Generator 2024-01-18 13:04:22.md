```english
max := 10

for i = 1 to max do
    for j = 1 to max do
        print(i * j)
        print(" ")
    end for
    print()
end for
```

Explanation:

1. `max := 10`: Set the maximum value for the outer and inner loops to 10.

2. `for i = 1 to max do`: Start the outer loop with `i` initialized to 1 and incrementing by 1 until it reaches `max`.

3. `for j = 1 to max do`: Inside the outer loop, start a nested inner loop with `j` initialized to 1 and incrementing by 1 until it reaches `max`.

4. `print(i * j)`: Inside the inner loop, calculate and print the product of `i` and `j`, effectively displaying the multiplication table.

5. `print(" ")`: After printing the product, print a space to separate the values.

6. `end for`: End the inner loop.

7. `print()`: After each outer loop iteration, print a newline to separate the rows of the multiplication table.

8. `end for`: End the outer loop.

Overall, this code generates a 10x10 multiplication table, displaying the products of numbers from 1 to 10 in a tabular format.