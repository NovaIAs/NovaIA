```forth
: Fib [\ n -- r ]
  1 1 Recurse Drop ;

: Recurse [ n -- r ]
  dup 0 =
  [ Drop 0 ]
  [ swap over - Fib + ]
  if ;

: Rock Paper Scissors [\ -- ]
  0 2 random Over ." $ "
  0 2 random Swap ." $ "
  8 rand > 2 rand < and
  [ ."Player Wins!" ]
  [ ."Computer Wins!" ]
  [ ."Tie!" ]
  if ;

: GCD [\ n m -- d ]
  [<]
  [ Swap ]
  [<]
  while Repeat Drop ;

: LCM [\ n m -- l ]
  Dup Dup GCD */ ;

: Greatest Common Divisor
  A B GCD OutString
  "Greatest common divisor of '$1' and '$2' is '$3'" EmitLn ;

: Least Common Multiple
  A B LCM OutString
  "Least common multiple of '$1' and '$2' is '$3'" EmitLn ;

: Fibonacci
  Number OutString
  "Fibonacci number at index '$1' is '$2'" EmitLn ;

: Random Number
  Rand OutString
  "Random number between 0 and 255 is '$1'" EmitLn ;

: Rock Paper Scissors
  "Rock, paper, scissors!" EmitLn
  RPSGame Play ;

: Text Reverse
  Str Reverse OutString
  "Reversed string is '$1'" EmitLn ;

: Palindrome Check
  Str Palyndromic? OutString
  "'$1' is a palindrome? '$2'" EmitLn ;

: Factorial
  Number OutString
  "Factorial of '$1' is '$2'" EmitLn ;

: Prime Check
  Number OutString
  "'$1' is prime? '$2'" EmitLn ;

: Perfect Number Check
  Number OutString
  "'$1' is perfect? '$2'" EmitLn ;

: Collatz Conjecture
  Number OutString
  "Collatz conjecture sequence starting at '$1':" EmitLn
  Collatz Run ;
```