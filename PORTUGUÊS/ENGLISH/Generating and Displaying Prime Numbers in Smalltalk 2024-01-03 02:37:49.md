Certainly! Here's a large and differentiated code written in Smalltalk:

```smalltalk
| numbersList resultSet |

numbersList := #(1 2 3 4 5 6 7 8 9 10).
resultSet := Set new.

numbersList do: [:each |
    | tempSet |

    tempSet := Set new.
    (2 to: each - 1) do: [:i |
        (each \\ i) = 0 ifTrue: [
            tempSet add: i
        ]
    ].

    tempSet isEmpty ifTrue: [
        resultSet add: each
    ]
].

resultSet do: [:primeNumber |
    Transcript show: 'Prime number: ', primeNumber asString; cr.
].
```

This code generates and prints all the prime numbers from a given list of numbers (1 to 10 in this case). It uses the `do:` loop to iterate over each number in the list. For each number, it checks if there are any divisors other than 1 and itself by using another `do:` loop with a counter `i` ranging from 2 to `each - 1`.

If a divisor is found, the code adds it to a temporary set `tempSet`. If the `tempSet` remains empty after the loop, it means the number is prime, and it is added to the final result set `resultSet`.

Finally, the code uses the `do:` loop again to print each prime number in the `resultSet` to the Transcript.

This code demonstrates basic looping, conditionals, and set operations in Smalltalk to identify and display prime numbers.