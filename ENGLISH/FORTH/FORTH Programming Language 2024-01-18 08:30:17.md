```forth
: fact ( n -- n! )
  1 if dup while dup * ;

: fib ( n -- nth fibonacci number )
  0 1 until [ drop dup + ] ;

: is-prime ( n -- flag )
  2 swap begin dup < while dup 1 + dup * over mod drop repeat ;

: gcd ( n m -- gcd )
  0 swap until [ dup mod swap ] ;

: to-binary ( n -- n2 )
  begin dup 2 > while 2mod drop 2 / repeat
  swap 2 * + ;

: from-binary ( n2 -- n )
  begin dup 0 > while 2 * + 2mod drop repeat ;

: char? ( c -- flag )
  32 126 between ;

: digit? ( c -- flag )
  48 57 between ;

: upper? ( c -- flag )
  65 90 between ;

: lower? ( c -- flag )
  97 122 between ;

: alpha? ( c -- flag )
  char? and [ upper? or lower? ] ;

: letter? ( c -- flag )
  alpha? or digit? ;

: is-palindrome ( str -- flag )
  [ 2dup length / 2dup 0 do swap i drop 1+ @ swap i @ = until ] ;

: echo ( ch -- )
  . ;

: get-char ( -- ch )
  key ;

: emit-string ( ch -- )
  begin dup 0 = until [ drop echo 1+ ] ;

: pretty-print ( n -- )
  begin dup 0 < while emit-char 32 emit repeat
  drop echo ;

: read-string ( -- str )
  [ begin 1- [ get-char emit-char dup 0 = ]
      while repeat
      drop 1+ ] ;

: greet ( -- )
  "Hello World" emit-string cr ;

: square ( n -- n^2 )
  dup * ;

: cube ( n -- n^3 )
  dup square * ;

: swap3 ( a b c -- c a b )
  dup over rot ;

: to-upper ( c -- c2 )
  begin dup 97 > and [ 32 - ] while
  drop 65 + ;

: to-lower ( c -- c2 )
  begin dup 65 > and [ 32 + ] while
  drop 97 + ;

: print-string ( str -- )
  begin dup 0 = until [ drop echo 1+ ] ;
```