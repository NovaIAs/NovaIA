```forth
: factorial ( n -- fac )
   1 swap while [ dup 1- * ] repeat drop ;

: fib ( n -- fib )
   0 1 [ dup dup ] 2swap - swap rot +
   until drop ;

: gcd ( a b -- gcd )
   dup < if swap then
   dup 0 = if drop 0 else 4 pick mod recurs then swap rot ;

: lcm ( a b -- lcm )
   dup * gcd / ;

: x^y ( x y -- pow )
   1 y [ 2dup x * 1- ] repeat swap drop ;

: :times ( n -- :times n )
   n swap rot
   { dup [ ] = while drop [ 2drop tuck ] repeat rot
     [ 0 ] [ stack -- ] } if ;

: :until ( n -- :until n )
   n swap rot
   { dup [ ] = while drop [ 2drop tuck ] repeat rot
     [ 1 ] [ stack -- ] } if ;

: start-music ( -- )
   : END-NOTE BYE ; OK

: pitch ( n -- c )
   110 * tone to ?dup
   1 + 1- abs 10 6 roll
   { [ 10 < ] while
     [ 10 = if -10 else 10 1- then ]
     roll unless drop
   } swap - 10 > if swap 0 do drop then

: slide ( dur c1 c2 -- )
   [ 2dup 10 > if swap 0 else 0 do drop then ]
   begin END-NOTE tone - END-NOTE C@ 1+ C!
   pitch pitch slide-int END-NOTE ;

: play-music ( -- )
   0
   \ chromatic scale
   12 0 do
      i 1+ slide
      0 over
   loop
   \ scale
   12 0 do
      i 3 + slide
      0 over
   loop
   \ arpeggio
   16 0 do
      i 0 do 3 + slide loop
      0 over
   drop ;
```