```forth
: Pythagorean-1 ( n a b -- c )
  dup square dup square + swap sqrt ;

: Pythagorean-2 ( n a b -- c )
  swap square + sqrt ;

: Pythagorean-3 ( n a -- b c )
  [ dup square dup square + swap sqrt ] ;

: Pythagorean-4 ( -- n a b c )
  1 1 2 begin
    Pythagorean-2 [ 2 - 2 + ] each
  until

: Pythagorean-5 ( n -- a b c )
  dup 1+ 2 /
  over dup 1- 2 /
  swap dup square dup square + swap sqrt ;

: Pythagorean-6 ( -- n a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 1- 2 /
  swap dup 1- 2 /
  swap dup square dup square + swap sqrt ;

: Pythagorean-7 ( n -- a b c )
  dup 1- 2 /
  dup [ dup 1+ square ] [ dup 1- square ]
  2swap - swap sqrt ;

: Pythagorean-8 ( -- n a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 1- 2 /
  [ swap dup 1- square ] [ swap dup 1+ square ]
  2swap - swap sqrt ;

: Pythagorean-9 ( -- n a b c )
  dup 1+ 2 /
  dup [ dup 1+ square ] [ dup 1- square ]
  2swap - sqrt ;

: Pythagorean-10 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 1+ 2 /
  [ dup dup 1+ square ] [ dup dup 1- square ]
  2swap - sqrt ;

: Pythagorean-11 ( n -- a b c )
  dup 1- 2 /
  dup [ dup 1+ square ] [ dup 1- square ]
  2swap + swap sqrt ;

: Pythagorean-12 ( -- n a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 1- 2 /
  [ dup dup 1+ square ] [ dup dup 1- square ]
  2swap + sqrt ;

: Pythagorean-13 ( n -- a b c )
  2 /
  [ dup dup - square ] [ dup 1- square ]
  2swap + sqrt ;

: Pythagorean-14 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  2 /
  [ dup dup - square ] [ dup 1- square ]
  2swap + sqrt ;

: Pythagorean-15 ( n -- a b c )
  2 /
  [ dup dup + square ] [ dup 1- square ]
  2swap + swap sqrt ;

: Pythagorean-16 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  2 /
  [ dup dup + square ] [ dup 1- square ]
  2swap + sqrt ;

: Pythagorean-17 ( n -- a b c )
  dup 2 /
  [ swap rot - square ] [ dup 1+ square ]
  2swap - swap sqrt ;

: Pythagorean-18 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 2 /
  [ swap rot - square ] [ dup 1+ square ]
  2swap - sqrt ;

: Pythagorean-19 ( n -- a b c )
  dup 2 /
  [ swap rot + square ] [ dup 1+ square ]
  2swap + swap sqrt ;

: Pythagorean-20 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 2 /
  [ swap rot + square ] [ dup 1+ square ]
  2swap + sqrt ;

: Pythagorean-21 ( n -- a b c )
  dup 2 /
  [ swap rot - square ] [ dup 1- square ]
  2swap - swap sqrt ;

: Pythagorean-22 ( n -- a b c )
  begin
    2dup <
  while
    [ 2dup > ]
  repeat
  dup 2 /
  [ swap rot - square ] [ dup 1- square ]
  2swap - sqrt ;
```