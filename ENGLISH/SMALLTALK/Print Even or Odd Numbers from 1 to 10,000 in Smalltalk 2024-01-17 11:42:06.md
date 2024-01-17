```smalltalk
(1 to: 10000) do: [ :i |
    (
        String streamContents: [ :s |
            s nextPutAll: 'The number ';
            s print: i;
            s nextPutAll: ' is ';
            s print: (i isEven ifTrue: [ 'even' ] ifFalse: [ 'odd' ]);
            s nextPutAll: '.'
        ]
    ) printNl
]
```

This code uses a loop to iterate through the numbers from 1 to 10,000. For each number, it uses a string stream to create a string that contains the number, whether it is even or odd, and a period. The string stream is then used to print the string to the console.

Here is a breakdown of the code:

* The `(1 to: 10000)` expression creates a range of numbers from 1 to 10,000.
* The `do:` method is used to iterate through the range of numbers. The `:i` parameter is the current number in the range.
* The `String streamContents:` method is used to create a string stream. The `:s` parameter is the string stream.
* The `s nextPutAll:` method is used to add a string to the string stream.
* The `s print:` method is used to print a value to the string stream.
* The `s nextPutAll:` method is used to add another string to the string stream.
* The `s printNl` method is used to print the string stream to the console.

The output of the code is a list of the numbers from 1 to 10,000, with each number followed by whether it is even or odd.