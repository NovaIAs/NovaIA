```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdvancedCodeExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Declare a delegate type for a function that takes an integer and returns a boolean
            delegate bool FilterFunction(int n);

            // Create a list of integers
            List<int> numbers = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            // Define a filter function to select even numbers
            FilterFunction isEven = (n) => n % 2 == 0;

            // Use the Where() method with the isEven delegate to filter the list
            List<int> evenNumbers = numbers.Where(isEven).ToList();

            // Print the even numbers
            Console.WriteLine("Even numbers:");
            foreach (int number in evenNumbers)
            {
                Console.WriteLine(number);
            }

            // Define a filter function to select numbers greater than 5
            FilterFunction greaterThan5 = (n) => n > 5;

            // Use the Where() method with the greaterThan5 delegate to filter the list
            List<int> numbersGreaterThan5 = numbers.Where(greaterThan5).ToList();

            // Print the numbers greater than 5
            Console.WriteLine("Numbers greater than 5:");
            foreach (int number in numbersGreaterThan5)
            {
                Console.WriteLine(number);
            }

            // Define a filter function to select numbers that are both even and greater than 5
            FilterFunction evenAndGreaterThan5 = (n) => isEven(n) && greaterThan5(n);

            // Use the Where() method with the evenAndGreaterThan5 delegate to filter the list
            List<int> evenAndGreaterThan5Numbers = numbers.Where(evenAndGreaterThan5).ToList();

            // Print the numbers that are both even and greater than 5
            Console.WriteLine("Numbers that are both even and greater than 5:");
            foreach (int number in evenAndGreaterThan5Numbers)
            {
                Console.WriteLine(number);
            }

            // Create a dictionary to store the occurrences of each number in the list
            Dictionary<int, int> numberOccurrences = new Dictionary<int, int>();

            // Iterate over the numbers list and update the occurrences dictionary
            foreach (int number in numbers)
            {
                if (numberOccurrences.ContainsKey(number))
                {
                    numberOccurrences[number]++;
                }
                else
                {
                    numberOccurrences[number] = 1;
                }
            }

            // Print the occurrences of each number
            Console.WriteLine("Occurrences of each number:");
            foreach (KeyValuePair<int, int> occurrence in numberOccurrences)
            {
                Console.WriteLine($"{occurrence.Key}: {occurrence.Value}");
            }

            // Create a list of strings to store the names of the months
            List<string> monthNames = new List<string>() { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };

            // Create a dictionary to store the number of days in each month
            Dictionary<string, int> daysInMonth = new Dictionary<string, int>();

            // Populate the dictionary with the number of days in each month
            daysInMonth["January"] = 31;
            daysInMonth["February"] = 28;
            daysInMonth["March"] = 31;
            daysInMonth["April"] = 30;
            daysInMonth["May"] = 31;
            daysInMonth["June"] = 30;
            daysInMonth["July"] = 31;
            daysInMonth["August"] = 31;
            daysInMonth["September"] = 30;
            daysInMonth["October"] = 31;
            daysInMonth["November"] = 30;
            daysInMonth["December"] = 31;

            // Print the number of days in each month
            Console.WriteLine("Number of days in each month:");
            foreach (KeyValuePair<string, int> month in daysInMonth)
            {
                Console.WriteLine($"{month.Key}: {month.Value}");
            }
        }
    }
}
```
Explanation:

1. We define a delegate type `FilterFunction` that takes an integer and returns a boolean. This allows us to pass a function as an argument to other functions.

2. We create a list of integers named `numbers` containing the numbers from 1 to 10.

3. We define a filter function `isEven` using a lambda expression. The lambda expression `(n) => n % 2 == 0` checks if a given integer `n` is even.

4. We use the `Where()` method with the `isEven` delegate to filter the `numbers` list and select only the even numbers. The result is stored in the `evenNumbers` list.

5. We print the even numbers using a `foreach` loop.

6. We define another filter function `greaterThan5` using a lambda expression. The lambda expression `(n) => n > 5` checks if a given integer `n` is greater than 5.

7. We use the `Where()` method with the `greaterThan5` delegate to filter the `numbers` list and select only the numbers greater than 5. The result is stored in the `numbersGreaterThan5` list.

8. We print the numbers greater than 5 using a `foreach` loop.

9. We define a third filter function `evenAndGreaterThan5` using a lambda expression. The lambda expression `(n) => isEven(n) && greaterThan5(n)` checks if a given integer `n` is both even and greater than 5.

10. We use the `Where()` method with the `evenAndGreaterThan5` delegate to filter the `numbers` list and select only the numbers that are both even and greater than 5. The result is stored in the `evenAndGreaterThan5Numbers` list.

11. We print the numbers that are both even and greater than 5 using a `foreach` loop.

12. We create a dictionary `numberOccurrences` to store the number of occurrences of each number in the `numbers` list.

13. We iterate over the `numbers` list and update the `numberOccurrences` dictionary accordingly.

14. We print the number of occurrences of each number using a `foreach` loop.

15. We create a list of strings `monthNames` containing the names of the months.

16. We create a dictionary `daysInMonth` to store the number of days in each month.

17. We populate the `daysInMonth` dictionary with the number of days in each month.

18. We print the number of days in each month using a `foreach` loop.