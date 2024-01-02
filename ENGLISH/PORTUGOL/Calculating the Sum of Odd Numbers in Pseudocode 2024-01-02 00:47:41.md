Certainly! Here's a complex and differentiated code in pseudocode (equivalent to PORTUGOL) with an explanation:

```
function calculateSum(n)
    if n <= 0 then
        return 0
    else if n % 2 == 0 then
        return calculateSum(n-1)
    else
        return n + calculateSum(n-2)

function main()
    input = readInput()
    sum = calculateSum(input)
    print("The sum of all odd numbers up to " + input + " is: " + sum)
```

Explanation:

The code consists of two parts: the `calculateSum` function and the `main` function. 

The `calculateSum` function takes an input `n` and recursively calculates the sum of all odd numbers up to `n`. Here's how it works:
1. If `n` is less than or equal to 0, the function returns 0 because there are no odd numbers to sum.
2. If `n` is an even number, the function calls itself with `n-1` to skip the even number. This step ensures that only odd numbers are considered for the sum.
3. If `n` is an odd number, the function adds `n` to the result of calling itself with `n-2`. This step includes the current odd number in the sum and continues the recursion with the next odd number.

The `main` function serves as the entry point of the program. It prompts the user for an input value, calls the `calculateSum` function with the input, and then displays the result.

Please note that this code is written in pseudocode, not in a specific programming language like PORTUGOL. You can easily translate it to any programming language of your choice.