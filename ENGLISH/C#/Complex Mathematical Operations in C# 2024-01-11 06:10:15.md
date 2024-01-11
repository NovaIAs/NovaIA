```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplexCSharpCode
{
    class Program
    {
        // Recursive function to calculate the factorial of a number
        static int Factorial(int n)
        {
            if (n == 0)
            {
                return 1;
            }
            else
            {
                return n * Factorial(n - 1);
            }
        }

        // Function to check if a number is prime
        static bool IsPrime(int n)
        {
            if (n <= 1)
            {
                return false;
            }
            else if (n <= 3)
            {
                return true;
            }
            else if (n % 2 == 0 || n % 3 == 0)
            {
                return false;
            }
            else
            {
                int i = 5;
                while (i * i <= n)
                {
                    if (n % i == 0 || n % (i + 2) == 0)
                    {
                        return false;
                    }
                    i += 6;
                }
                return true;
            }
        }

        // Function to find all the prime factors of a number
        static List<int> PrimeFactors(int n)
        {
            List<int> primeFactors = new List<int>();
            for (int i = 2; i <= n; i++)
            {
                if (n % i == 0 && IsPrime(i))
                {
                    primeFactors.Add(i);
                }
            }
            return primeFactors;
        }

        // Function to find the greatest common divisor of two numbers
        static int GreatestCommonDivisor(int a, int b)
        {
            if (b == 0)
            {
                return a;
            }
            else
            {
                return GreatestCommonDivisor(b, a % b);
            }
        }

        // Function to find the least common multiple of two numbers
        static int LeastCommonMultiple(int a, int b)
        {
            return (a * b) / GreatestCommonDivisor(a, b);
        }

        // Main method
        static void Main(string[] args)
        {
            // Calculate the factorial of a number
            int factorial = Factorial(5);
            Console.WriteLine("Factorial of 5: " + factorial);

            // Check if a number is prime
            bool isPrime = IsPrime(13);
            Console.WriteLine("Is 13 prime: " + isPrime);

            // Find all the prime factors of a number
            List<int> primeFactors = PrimeFactors(12);
            Console.WriteLine("Prime factors of 12: " + string.Join(", ", primeFactors));

            // Find the greatest common divisor of two numbers
            int gcd = GreatestCommonDivisor(12, 18);
            Console.WriteLine("Greatest common divisor of 12 and 18: " + gcd);

            // Find the least common multiple of two numbers
            int lcm = LeastCommonMultiple(12, 18);
            Console.WriteLine("Least common multiple of 12 and 18: " + lcm);
        }
    }
}
```

Explanation:

This C# code performs various complex mathematical operations on integers. Here's a breakdown of the code:

1. Recursive Function `Factorial`:
   - This function calculates the factorial of a given integer `n` using recursion.
   - The factorial of a number is the product of all positive integers from 1 to that number.
   - The function uses the recursive formula: `Factorial(n) = n * Factorial(n-1)`.

2. Function `IsPrime`:
   - This function checks if a given integer `n` is prime.
   - It follows a basic algorithm to determine primality:
     - If `n` is less than or equal to 1, it's not prime.
     - If `n` is less than or equal to 3, it's prime.
     - For other values of `n`, it checks divisibility by 2 and 3. If `n` is divisible by either, it's not prime.
     - It then checks divisibility by numbers of the form 6k ± 1 (i.e., 5, 7, 11, 13, ...) up to the square root of `n`. If `n` is divisible by any of these numbers, it's not prime.
     - If `n` passes all these checks, it's prime.

3. Function `PrimeFactors`:
   - This function finds all the prime factors of a given integer `n`.
   - It iterates through numbers from 2 to `n` and checks if each number is a prime factor of `n`.
   - If a number is a prime factor, it is added to the list of prime factors.

4. Function `GreatestCommonDivisor`:
   - This function calculates the greatest common divisor (GCD) of two given integers `a` and `b`.
   - It uses the Euclidean algorithm to find the GCD.
   - The algorithm repeatedly divides the larger number by the smaller number and takes the remainder.
   - When the remainder is 0, the last non-zero remainder is the GCD.

5. Function `LeastCommonMultiple`:
   - This function calculates the least common multiple (LCM) of two given integers `a` and `b`.
   - It uses the formula: LCM(a, b) = (a * b) / GCD(a, b).
   - It first calculates the GCD using the `GreatestCommonDivisor` function and then computes the LCM.

In the `Main` method, the code demonstrates the usage of these functions by performing various calculations and printing the results to the console.