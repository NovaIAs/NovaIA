```forth
: fib ( -- n )                      \ Fib of given number n
  dup 2 <=                          \ If n less than or equal to 2
  dup 1 =                          \ if n is 1
  if 1                              \  then: return 1
  recurse +                        \ Otherwise: return Fib(n-1) + Fib(n-2)
;

: showfib ( -- )                     \ Print Fib of numbers 0 to 9
  10 0 do i fib . loop            \ Do Fibs for 0 to 9 and print
;

: neg ( n -- )                      \ Negate a number
  swap -                            \ Subtract the number from 0
;

: abs ( n -- n )                    \ Absolute value
  neg over max                      \ Negate nbr and find max with original
;

: gcd ( m n -- gcd )               \ Greatest common divisor
  abs dup >                          \ Make sure n is >= m
  if swap else drop nop fi          \ Drop m if n was smaller
  while n 0 > do m mod n swap loop  \ Use Euclidean algorithm
  abs                               \ Return the gcd
;

: lcm ( m n -- lcm )               \ Least common multiple
  gcd * div                         \ lcm = m*n / gcd
;

: swap-if ( x y -- x y )            \ Swap two numbers if x < y
  < swap                           \ Swap if x is smaller
;

: not ( f -- t/f )                 \ Boolean negation
  dup 0=                            \ True if number is 0, false otherwise
;

: or ( f1 f2 -- f )                \ Boolean OR
  dup if drop true else over then  \ Return first if true, else second
;

: and ( f1 f2 -- f )               \ Boolean AND
  not or                            \ Do OR of negations
;

: drop-duplicates ( tbl -- )        \ Remove duplicates from a list
  begin true while
    dup swap over =                 \ If equal to prev element
    if drop else swap drop then     \  then: drop, else: leave and continue
  repeat drop                        \ Drop the last element
;

: stride ( tbl n -- tbl )           \ Skip n elements from a list
  begin dup 0 > while drop repeat  \ Drop first n elements
;

: irange ( start end -- )          \ Inclusive range on stack
  dup - 1 + swap do i loop dup drop \ Push start-end range, inclusive
;

: sort ( tbl -- )                  \ Sort a list
  begin
    dup 0 >  \ End if list is empty
    while
      2dup <  \ Find minimum element
      if dup swap else drop nop then
      dup stride rot stride swap      \ Remove min element from list
      swap drop swap               \ Put min element to the beginning
    repeat drop                      \ Drop the last element
  repeat
;

: print-range ( start end -- )     \ Print a range of numbers
  begin
    dup < while                      \ End if start > end
      dup @ .                       \ Print current value
      1+ swap                        \ Increment counter
    repeat drop                      \ Drop the last element
  repeat
;

: draw-line ( x1 y1 x2 y2 -- )     \ Draw a line using Bresenham's algorithm
  dup dup < if sign . swap else nip fi \ Draw an X line if X direction
  abs swap dup < if sign @ @ dup . else nip swap nip fi \ Same for Y
  abs > if swap else nip fi           \ And draw a Y line if Y is longer
  over dup 2 > if neg swap fi         \ If X is long do X step
  over dup 2 < if neg fi              \ If Y is short do Y step
  over dup 1+ swap rot swap rot      \ Increment step sizes
  2dup > if swap nip else nip swap fi \ Check which step is shorter
  do                                 \ And continue drawing
    over dup > if nip else swap over @ @ nip . fi
    over dup > if nip else swap drop nip fi
  loop drop                          \ Drop the last element
;

: main ( -- )                       \ Define and call "main"
  fib showfib                        \ Show Fibonacci numbers 0 to 9
  9 -5 gcd .                         \ Print gcd of 9 and -5
  9 -5 lcm .                         \ Print lcm of 9 and -5
  to-ascii 'a' .                     \ Print the ascii code of 'a'
  149 abs .                          \ Print absolutes value of 149
  'abcdef' 3 swap-if .               \ Print 'bcdef'
  1 0 swap-if .                      \ Print 0
  true dup not .                      \ Print false
  'a' 'b' 'c' 'd' 'e' not or .      \ Print true
  'a' 'b' 'c' 'd' 'e' and .         \ Print false
  2 3 1 5 2 4 2 1 3 4 5 6 drop-duplicates . \ Print 123456
  'abcde' 3 stride .                 \ Print 'cde'
  1 10 irange print-range            \ Print inclusive range of 1 to 10
  'a' 'b' 'c' 'd' 'e' sort .         \ Print sorted list
  3 8 draw-line                       \ Draw a line from (3,8) to (5,2)
;
```

Explanations:

* `fib`: Calculates the Fibonacci number of a given number.
* `showfib`: Prints the Fibonacci numbers from 0 to 9.
* `neg`: Negates a number.
* `abs`: Calculates the absolute value of a number.
* `gcd`: Calculates the greatest common divisor of two numbers.
* `lcm`: Calculates the least common multiple of two numbers.
* `swap-if`: Swaps two numbers if the first is less than the second.
* `not`: Negates a boolean value.
* `or`: Performs a boolean OR operation.
* `and`: Performs a boolean AND operation.
* `drop-duplicates`: Removes duplicate elements from a list.
* `stride`: Skips a specified number of elements from a list.
* `irange`: Creates an inclusive range of numbers on the stack.
* `sort`: Sorts a list of numbers in ascending order.
* `print-range`: Prints a range of numbers.
* `draw-line`: Draws a line using Bresenham's algorithm.
* `main`: Defines and calls the "main" function, which includes all the other functions and their calls.