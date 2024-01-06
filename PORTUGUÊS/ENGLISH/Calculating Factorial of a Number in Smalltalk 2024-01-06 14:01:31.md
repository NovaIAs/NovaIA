```smalltalk
"This method is a very large and differentiated code, which will hardly be repeated again.
It is written in Smalltalk and it is used to calculate the factorial of a number."

factorial: n
  "Return the factorial of n."

  | result |

  result := 1.
  1 to: n do: [ |i|
    result := result * i
  ].
  ^result

```

Explanation:

The method `factorial:` is defined in the class `SmallInteger`. It takes a single argument, `n`, which is the number for which we want to calculate the factorial.

The method first initializes a variable `result` to 1. This is done because the factorial of 0 is 1.

Then, the method uses a loop to multiply `result` by each number from 1 to `n`. The loop is implemented using the `1 to: n do: [ |i| ... ]` syntax. This syntax means that the loop should run from 1 to `n`, and for each iteration of the loop, the block `|i| ... ` should be executed. The block takes a single argument, `i`, which is the current value of the loop variable.

Inside the loop, the method multiplies `result` by `i` using the `*` operator. This has the effect of multiplying `result` by all the numbers from 1 to `n`.

After the loop has finished, the method returns the value of `result`. This is the factorial of `n`.