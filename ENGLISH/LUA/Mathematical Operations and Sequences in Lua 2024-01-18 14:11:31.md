```
-- Define the function that will be used to calculate the factorial of a number
function factorial(n)
   if n == 0 then
      return 1 -- Base case: factorial of 0 is 1
   else
      return n * factorial(n-1) -- Recursive case: factorial of n is n * factorial(n-1)
   end
end

-- Create a table to store the factorial values for numbers from 0 to 10
factorialValues = {}
for i = 0, 10 do
   factorialValues[i] = factorial(i)
end

-- Print the factorial values for numbers from 0 to 10
print("Factorial values:")
for i = 0, 10 do
   print(i .. "! = " .. factorialValues[i])
end

-- Define a function to check if a number is prime
function isPrime(n)
   if n <= 1 then
      return false -- Base case: 1 is not prime
   end

   for i = 2, n-1 do
      if n % i == 0 then
         return false -- If n is divisible by any number from 2 to n-1, it is not prime
      end
   end

   return true -- If n is not divisible by any number from 2 to n-1, it is prime
end

-- Create a table to store the prime numbers from 1 to 100
primeNumbers = {}
for i = 1, 100 do
   if isPrime(i) then
      primeNumbers[#primeNumbers + 1] = i -- Add the prime number to the table
   end
end

-- Print the prime numbers from 1 to 100
print("Prime numbers from 1 to 100:")
for i = 1, #primeNumbers do
   print(primeNumbers[i])
end

-- Define a function to calculate the Fibonacci sequence up to a certain number
function fibonacci(n)
   if n == 0 or n == 1 then
      return n -- Base case: fibonacci of 0 or 1 is 0 or 1
   else
      return fibonacci(n-1) + fibonacci(n-2) -- Recursive case: fibonacci of n is fibonacci of n-1 + fibonacci of n-2
   end
end

-- Create a table to store the Fibonacci sequence values for numbers from 0 to 10
fibonacciValues = {}
for i = 0, 10 do
   fibonacciValues[i] = fibonacci(i)
end

-- Print the Fibonacci sequence values for numbers from 0 to 10
print("Fibonacci sequence values:")
for i = 0, 10 do
   print(i .. ": " .. fibonacciValues[i])
end
```

This code performs several mathematical calculations and operations, including calculating factorials, checking if a number is prime, generating the prime numbers from 1 to 100, calculating the Fibonacci sequence up to a certain number, and printing the results. It demonstrates the use of functions, loops, tables, and conditional statements in Lua.