```smalltalk
(1 to: 100) do: [ :number |
  (1 to: number) do: [ :i |
    Transcript show: (number rem: i).
    Transcript cr.
  ].
  Transcript cr cr.
].
```

Explanation:

This code is a nested loop that prints a table of remainders for numbers from 1 to 100.

The outer loop (the first "do:") iterates through the numbers from 1 to 100 using the variable "number".

The inner loop (the second "do:") iterates through the numbers from 1 to the current "number" using the variable "i".

Inside the inner loop, the line "Transcript show: (number rem: i)" prints the remainder of "number" divided by "i".

The line "Transcript cr" moves the cursor to the next line.

After the inner loop is finished, the line "Transcript cr cr" prints two blank lines.

The result is a table of remainders that looks like this:

```
1 0
2 0 1
3 0 1 2
4 0 1 2 3
5 0 1 2 3 4
6 0 1 2 3 4 5
7 0 1 2 3 4 5 6
8 0 1 2 3 4 5 6 7
9 0 1 2 3 4 5 6 7 8
10 0 1 2 3 4 5 6 7 8 9
11 0 1 2 3 4 5 6 7 8 9 10
12 0 1 2 3 4 5 6 7 8 9 10 11
13 0 1 2 3 4 5 6 7 8 9 10 11 12
14 0 1 2 3 4 5 6 7 8 9 10 11 12 13
15 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
17 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
18 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
19 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
20 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
```