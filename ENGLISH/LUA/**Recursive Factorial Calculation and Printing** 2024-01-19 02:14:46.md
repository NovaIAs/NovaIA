```lua
-- This Lua program computes the factorial of a non-negative integer n.
-- Returns 1 if n is 0, n * factorial(n-1) otherwise.

function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- Print the factorials of the first 10 non-negative integers.
for i = 0, 10 do
  print(i .. "! = " .. factorial(i))
end
```

Explanation:

1. `function factorial(n)`: Defines a function named `factorial` that takes one argument, `n`. This function computes the factorial of `n`.

2. `if n == 0 then`: Checks if `n` is equal to 0. If it is, the function returns 1 because the factorial of 0 is defined to be 1.

3. `else`: If `n` is not equal to 0, the function calculates the factorial using recursion. It multiplies `n` by the factorial of `n-1`. This process continues until `n` reaches 0, at which point the recursion stops and the function returns the result.

4. `for i = 0, 10 do`: This loop iterates through the non-negative integers from 0 to 10.

5. `print(i .. "! = " .. factorial(i))`: Inside the loop, it prints the factorial of each integer. It uses string concatenation (`..`) to format the output in a readable way.

When you run this program, it will print the factorials of the first 10 non-negative integers. The output should look something like this:

```
0! = 1
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880
10! = 3628800
```

This code showcases recursion, loops, and string concatenation, making it a bit more complex and varied compared to simple scripts.