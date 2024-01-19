```smalltalk
"Hello World!" displayNl.
```

Explanation:

* `"Hello World!"` is a string literal, which is a sequence of characters enclosed in double quotes.
* `displayNl.` is a method call to the `displayNl` method of the `String` class, which prints the string to the console followed by a newline.

This code will print "Hello World!" to the console.

```smalltalk
1 to: 10 do: [ :n | n displayNl. ].
```

Explanation:

* `1 to: 10` is a range literal, which represents all the integers from 1 to 10.
* `do: [ :n | n displayNl. ]` is a block of code that is executed for each element in the range. The block takes a single parameter, `:n`, which represents the current element in the range.
* `n displayNl.` is a method call to the `displayNl` method of the `Integer` class, which prints the integer to the console followed by a newline.

This code will print the numbers from 1 to 10 to the console, each on its own line.

```smalltalk
[ 1, 2, 3, 4, 5 ] do: [ :n | n displayNl. ].
```

Explanation:

* `[ 1, 2, 3, 4, 5 ]` is an array literal, which is a collection of elements enclosed in square brackets.
* `do: [ :n | n displayNl. ]` is a block of code that is executed for each element in the array. The block takes a single parameter, `:n`, which represents the current element in the array.
* `n displayNl.` is a method call to the `displayNl` method of the `Integer` class, which prints the integer to the console followed by a newline.

This code will print the numbers from 1 to 5 to the console, each on its own line.

```smalltalk
"Hello " , "World!" displayNl.
```

Explanation:

* `"Hello "` and `"World!"` are string literals, which are sequences of characters enclosed in double quotes.
* `,` is a string concatenation operator, which joins two strings together.
* `displayNl.` is a method call to the `displayNl` method of the `String` class, which prints the string to the console followed by a newline.

This code will print "Hello World!" to the console.

```smalltalk
10 + 20 - 30 * 40 / 50
```

Explanation:

* `+`, `-`, `*`, and `/` are arithmetic operators, which perform addition, subtraction, multiplication, and division, respectively.
* `10`, `20`, `30`, `40`, and `50` are integer literals, which represent numeric values.

This code will evaluate the expression `10 + 20 - 30 * 40 / 50` and print the result to the console. The result of the expression is -20.