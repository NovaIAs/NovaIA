```portugol
program ComplexCode;

function isPrime(number);
   for number = 2 to number-1 do
     if number mod number = 0 then
       return false;
     end if;
   end for;
   return true;
end function;

function fibonacci(number);
   if number <= 1 then
     return number;
   end if;
   return fibonacci(number-1) + fibonacci(number-2);
end function;

procedure printFactors(number);
   print("Factors of ", number, ":");
   for factor = 1 to number do
     if number mod factor = 0 then
       print(factor);
     end if;
   end for;
   print("\n");
end procedure;

function gcd(a, b);
   while b <> 0 do
     temp = b;
     b = a mod b;
     a = temp;
   end while;
   return a;
end function;

function lcm(a, b);
   return (a * b) / gcd(a, b);
end function;

function primeFactors(number);
   factors = [];
   for factor = 2 to floor(sqrt(number)) do
     while number mod factor = 0 do
       factors.push_back(factor);
       number /= factor;
     end while;
   end for;
   if number > 1 then
     factors.push_back(number);
   end if;
   return factors;
end function;

function isKaprekar(number);
   squared = (number * number);
   string_squared = string(squared);
   length = string_squared.length;
   left_part = int(string_squared.substr(0, length / 2));
   right_part = int(string_squared.substr(length / 2, length));
   return left_part + right_part == number;
end function;

procedure testCode();
   print("Prime Numbers:");
   for number = 1 to 100 do
     if isPrime(number) then
       print(number, ' ');
     end if;
   end for;

   print("\nFibonacci Sequence:");
   for number = 0 to 10 do
     print(fibonacci(number), ' ');
   end for;

   print("\nFactors of 12:");
   printFactors(12);

   print("\nGreatest Common Divisor (GCD) of 100 and 120:");
   print(gcd(100, 120));

   print("\nLeast Common Multiple (LCM) of 100 and 120:");
   print(lcm(100, 120));

   print("\nPrime Factors of 120:");
   factors = primeFactors(120);
   for factor in factors do
     print(factor, ' ');
   end for;

   print("\nIs 45 a Kaprekar Number?:");
   print(isKaprekar(45));
end procedure;

testCode();
```

Explanation:

This code demonstrates the implementation of various mathematical functions and algorithms in PORTUGOL. Here's a breakdown of each function and procedure:

1. `isPrime`: Checks if a given number is prime (divisible by 1 and itself only) using a loop.

2. `fibonacci`: Calculates the nth Fibonacci number using a recursive function.

3. `printFactors`: Prints all the factors of a given number.

4. `gcd`: Computes the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.

5. `lcm`: Determines the least common multiple (LCM) of two numbers using GCD.

6. `primeFactors`: Generates a list of prime factors for a given number.

7. `isKaprekar`: Checks if a number is a Kaprekar number, where the square of the number can be split into two parts that sum to the original number.

8. `testCode`: Calls each of the functions and procedures to showcase their functionality.

In the end, it demonstrates the usage of these functions and procedures by printing out prime numbers, the Fibonacci sequence, factors of a number, GCD, LCM, prime factors, and checking for Kaprekar numbers.