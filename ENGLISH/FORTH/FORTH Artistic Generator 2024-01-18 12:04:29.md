```forth
: fib ( n -- n-th fibonacci number )
    0 swap 1 2do dup >r r> - rdrop r> + loop drop ;

: draw-fib ( n x y -- n-th fibonacci number x y )
    dup >r >r fib over over 4 * x y + swap r> - rdrop r> + rdrop rdrop
    dup 2dup @ type space -1 3 * @ swap do dup @ swap space type loop ;

: next-fib ( n -- next fibonacci number )
    dup fib + ;

: generate-fibs ( n -- n fibonacci numbers )
    dup 1 do i next-fib loop drop ;

: print-fibs ( n -- )
    ." Fibonacci sequence of length " . number cr
    0 80 do i "----." loop cr
    dup generate-fibs [ ] n pick
    0 swap 20 do i
        dup 10 >= if drop leave then
        dup i swap 2drop @ draw-fib loop
    loop drop ;

: fib-line ( n -- n fibonacci numbers )
    dup 10 >= if drop 10 else then
    dup next-fib
    dup 10 >= if drop 10 else then
    dup [ ] swap generate-fibs ;

: draw-line ( n start-x start-y -- n fibonacci numbers start-x start-y )
    dup >r >r swap 3dup 0
    swap 2dup [char] type space -1 3 * @ swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop
    2dup dup 1+ 2dup dup [ ] swap generate-fibs [char] type space -1 3 * @
    swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop
    2dup dup 2+ -1 2dup dup [ ] swap generate-fibs [char] type space -1 3 * @
    swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop cr ;

: print-lines ( n -- )
    ." Fibonacci sequence of length " . number cr
    0 80 do i "----." loop cr
    dup generate-fibs 0 swap + 10 do i fib-line loop drop ;

: game-of-life ( -- )
    ." Conway's Game of Life" cr
    0 80 do i "----." loop cr 2dup do
        0 24 do j
            2 random 2 random
            2 swap pick if [ ] else [char] then swap if
                ."  "
            else
                ."[]"
            then
        loop cr
    loop drop ;

: generate-random-lines ( n -- n random lines )
    dup 10 >= if drop 10 else then
    dup next-fib
    dup 10 >= if drop 10 else then
    dup [ ] swap generate-fibs ;

: draw-random-line ( n start-x start-y -- n fibonacci numbers start-x start-y )
    dup >r >r swap 3dup 0
    swap 2dup [char] type space -1 3 * @ swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop
    2dup dup 1+ 2dup dup [ ] swap generate-random-lines [char] type space -1 3 * @
    swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop
    2dup dup 2+ -1 2dup dup [ ] swap generate-random-lines [char] type space -1 3 * @
    swap do dup @ swap space type loop
    nip swap swap r> - rdrop r> + rdrop rdrop cr ;

: print-random-lines ( n -- )
    ." Fibonacci sequence of length " . number cr
    0 80 do i "----." loop cr
    dup generate-random-lines 0 swap + 10 do i draw-random-line loop drop ;

```