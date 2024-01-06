```forth
: FIB ( n -- n-th fibonacci number )
  0 swap 1 = if drop 1 then
  1 swap 1 swap + fib fib + ;

: FIB-SERIES ( n -- )
  0 do
    i fib . cr
  loop ;

: FIB-TABLE ( n -- )
  0 do
    i 20 spaces fib . cr
  loop ;

: RANDOM ( n -- )
  random 1+ ;

: RANDOM-LIST ( n -- )
  n random-list ;

: RANDOM-TABLE ( n -- )
  0 do
    i random-list 20 spaces . cr
  loop ;

: COINS ( n -- )
  n 10 base . " cents" cr ;

: BUCKS ( n -- )
  n 100 base . " dollars" cr ;

: CELL ( n -- )
  s" cell = " . cr
  @ . cr ;

: CELL+ ( n -- )
  s" cell + 1 = " . cr
  1 + @ . cr ;

: CELL- ( n -- )
  s" cell - 1 = " . cr
  1 - @ . cr ;

: SQUARE ( n -- )
  dup * . cr ;

: CUBE ( n -- )
  dup dup * * . cr ;

: FACTORIAL ( n -- )
  1 do
    i 1 > while
      i 1 - *
    repeat
    drop ;

: GREATEST-COMMON-DIVISOR ( a b -- gcd )
  over mod 0 =
  if drop swap
    then
  while
    dup over mod swap
    repeat
    drop ;

: LEAST-COMMON-MULTIPLE ( a b -- lcm )
  over mod 0 =
  if drop swap
    then
  while
    dup over mod swap
    repeat
    drop * ;

: IS-PRIME ( n -- f )
  2 2 swap mod 0 =
  if 0
    then
  2 > while
    i 2 + swap mod 0 =
    drop
  repeat
  drop 1 ;

: PRIME-LIST ( n -- )
  2 do
    i is-prime
    if . cr then
  loop ;

: REVERSE ( s -- )
  dup length 0 do
    i c@ swap swap tuck
  loop
  drop ;

: PALLINDROME ( s -- f )
  dup length 2 / 0 do
    i c@ swap swap tuck
  loop
  drop = ;

: ROTATE ( n s -- )
  dup length > and
  if
    dup n > and
    if
      -rot
    else
      rot
    then
  else
    drop
  then ;

: IS-DIGIT ( c -- f )
  '0 c < '9 c > or ;

: IS-UPPER ( c -- f )
  'A c < 'Z c > ;

: IS-LOWER ( c -- f )
  'a c < 'z c > ;

: IS-ALPHA ( c -- f )
  is-upper or is-lower ;

: IS-ALNUM ( c -- f )
  is-digit or is-alpha ;

: IS-SPACE ( c -- f )
  ' ' c = or cr c = or lf c = ;

: TO-UPPER ( c -- )
  is-lower and
  if
    dup 'a - 'A +
  then ;

: TO-LOWER ( c -- )
  is-upper and
  if
    dup 'A - 'a +
  then ;

: CHAR ( n -- c )
  swap 0 do
    i dup char
  loop
  drop ;

: ASCII ( c -- n )
  dup char c@ swap 0 do
    i over swap mod 10 +
  loop
  drop ;

: PRINT-STR ( s -- )
  dup length 0 do
    i c@ put
  loop
  drop ;

: FIND ( s1 s2 -- i )
  dup length 0 do
    i swap c@ dup s2 0 do
      j swap c@ =
      if drop i
        then
    loop
    drop -1 ;

: REPLACE ( s1 s2 n -- )
  dup length 0 do
    i swap c@ swap
    n > and
    if
      s2 i c@ =
        if drop i
          then
    then
  loop
  drop ;

: COUNT-CHAR ( s c -- n )
  dup length 0 do
    i swap c@ dup c =
    if
      1 +
    then
  loop
  drop ;

: COUNT-STR ( s1 s2 -- n )
  dup length 0 do
    i swap c@ swap s2 0 do
      j swap c@ =
      if
        1 +
      then
    loop
    drop ;
```