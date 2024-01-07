```c#
// This program calculates the sum of the first 100 prime numbers.

// Create a list to store the prime numbers.
List<int> primeNumbers = new List<int>();

// Initialize the first prime number.
int currentPrime = 2;

// While the list of prime numbers has less than 100 elements, keep searching for prime numbers.
while (primeNumbers.Count < 100)
{
    // Check if the current number is prime.
    bool isPrime = true;
    for (int i = 2; i <= Math.Sqrt(currentPrime); i++)
    {
        if (currentPrime % i == 0)
        {
            isPrime = false;
            break;
        }
    }

    // If the current number is prime, add it to the list of prime numbers.
    if (isPrime)
    {
        primeNumbers.Add(currentPrime);
    }

    // Increment the current number.
    currentPrime++;
}

// Calculate the sum of the prime numbers.
int sumOfPrimeNumbers = 0;
foreach (int primeNumber in primeNumbers)
{
    sumOfPrimeNumbers += primeNumber;
}

// Print the sum of the prime numbers.
Console.WriteLine($"The sum of the first 100 prime numbers is {sumOfPrimeNumbers}.");
```

This code first creates a list to store the prime numbers. Then, it initializes the first prime number to 2. The code then enters a while loop that continues until the list of prime numbers has 100 elements. Inside the loop, the code checks if the current number is prime by dividing it by all the numbers from 2 to the square root of the current number. If the current number is divisible by any of these numbers, it is not prime and the loop continues to the next number. If the current number is not divisible by any of these numbers, it is prime and the code adds it to the list of prime numbers. The code then increments the current number and continues to the next number.

Once the loop has finished, the code calculates the sum of the prime numbers by adding them all together. Finally, the code prints the sum of the prime numbers to the console.