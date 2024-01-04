```FORTH
: make-adder ( value -- addr )
  dup 2dup
  >r over -
  2 / +
  r> , ;

: adder
  [ swap ] make-adder over + ;

: 1+ ( -- value )
  [ dup 1 + ] make-adder ;

: 1- ( -- value )
  [ dup 1 - ] make-adder ;

: 2+ ( -- value )
  [ dup 2 + ] make-adder ;

: 2- ( -- value )
  [ dup 2 - ] make-adder ;

: 3+ ( -- value )
  [ dup 3 + ] make-adder ;

: 3- ( -- value )
  [ dup 3 - ] make-adder ;

: 4+ ( -- value )
  [ dup 4 + ] make-adder ;

: 4- ( -- value )
  [ dup 4 - ] make-adder ;

: 5+ ( -- value )
  [ dup 5 + ] make-adder ;

: 5- ( -- value )
  [ dup 5 - ] make-adder ;

: 6+ ( -- value )
  [ dup 6 + ] make-adder ;

: 6- ( -- value )
  [ dup 6 - ] make-adder ;

: 7+ ( -- value )
  [ dup 7 + ] make-adder ;

: 7- ( -- value )
  [ dup 7 - ] make-adder ;

: 8+ ( -- value )
  [ dup 8 + ] make-adder ;

: 8- ( -- value )
  [ dup 8 - ] make-adder ;

: 9+ ( -- value )
  [ dup 9 + ] make-adder ;

: 9- ( -- value )
  [ dup 9 - ] make-adder ;

: 10+ ( -- value )
  [ dup 10 + ] make-adder ;

: 10- ( -- value )
  [ dup 10 - ] make-adder ;

: 11+ ( -- value )
  [ dup 11 + ] make-adder ;

: 11- ( -- value )
  [ dup 11 - ] make-adder ;

: 12+ ( -- value )
  [ dup 12 + ] make-adder ;

: 12- ( -- value )
  [ dup 12 - ] make-adder ;

: 13+ ( -- value )
  [ dup 13 + ] make-adder ;

: 13- ( -- value )
  [ dup 13 - ] make-adder ;

: 14+ ( -- value )
  [ dup 14 + ] make-adder ;

: 14- ( -- value )
  [ dup 14 - ] make-adder ;

: 15+ ( -- value )
  [ dup 15 + ] make-adder ;

: 15- ( -- value )
  [ dup 15 - ] make-adder ;

: 16+ ( -- value )
  [ dup 16 + ] make-adder ;

: 16- ( -- value )
  [ dup 16 - ] make-adder ;

: 17+ ( -- value )
  [ dup 17 + ] make-adder ;

: 17- ( -- value )
  [ dup 17 - ] make-adder ;

: 18+ ( -- value )
  [ dup 18 + ] make-adder ;

: 18- ( -- value )
  [ dup 18 - ] make-adder ;

: 19+ ( -- value )
  [ dup 19 + ] make-adder ;

: 19- ( -- value )
  [ dup 19 - ] make-adder ;

: 20+ ( -- value )
  [ dup 20 + ] make-adder ;

: 20- ( -- value )
  [ dup 20 - ] make-adder ;

: 21+ ( -- value )
  [ dup 21 + ] make-adder ;

: 21- ( -- value )
  [ dup 21 - ] make-adder ;

: 22+ ( -- value )
  [ dup 22 + ] make-adder ;

: 22- ( -- value )
  [ dup 22 - ] make-adder ;

: 23+ ( -- value )
  [ dup 23 + ] make-adder ;

: 23- ( -- value )
  [ dup 23 - ] make-adder ;

: 24+ ( -- value )
  [ dup 24 + ] make-adder ;

: 24- ( -- value )
  [ dup 24 - ] make-adder ;

: 25+ ( -- value )
  [ dup 25 + ] make-adder ;

: 25- ( -- value )
  [ dup 25 - ] make-adder ;

: 26+ ( -- value )
  [ dup 26 + ] make-adder ;

: 26- ( -- value )
  [ dup 26 - ] make-adder ;

: 27+ ( -- value )
  [ dup 27 + ] make-adder ;

: 27- ( -- value )
  [ dup 27 - ] make-adder ;

: 28+ ( -- value )
  [ dup 28 + ] make-adder ;

: 28- ( -- value )
  [ dup 28 - ] make-adder ;

: 29+ ( -- value )
  [ dup 29 + ] make-adder ;

: 29- ( -- value )
  [ dup 29 - ] make-adder ;

: 30+ ( -- value )
  [ dup 30 + ] make-adder ;

: 30- ( -- value )
  [ dup 30 - ] make-adder ;

: 31+ ( -- value )
  [ dup 31 + ] make-adder ;

: 31- ( -- value )
  [ dup 31 - ] make-adder ;

: 32+ ( -- value )
  [ dup 32 + ] make-adder ;

: 32- ( -- value )
  [ dup 32 - ] make-adder ;

: 33+ ( -- value )
  [ dup 33 + ] make-adder ;

: 33- ( -- value )
  [ dup 33 - ] make-adder ;

: 34+ ( -- value )
  [ dup 34 + ] make-adder ;

: 34- ( -- value )
  [ dup 34 - ] make-adder ;

: 35+ ( -- value )
  [ dup 35 + ] make-adder ;

: 35- ( -- value )
  [ dup 35 - ] make-adder ;

: 36+ ( -- value )
  [ dup 36 + ] make-adder ;

: 36- ( -- value )
  [ dup 36 - ] make-adder ;

: 37+ ( -- value )
  [ dup 37 + ] make-adder ;

: 37- ( -- value )
  [ dup 37 - ] make-adder ;

: 38+ ( -- value )
  [ dup 38 + ] make-adder ;

: 38- ( -- value )
  [ dup 38 - ] make-adder ;

: 39+ ( -- value )
  [ dup 39 + ] make-adder ;

: 39- ( -- value )
  [ dup 39 - ] make-adder ;

: 40+ ( -- value )
  [ dup 40 + ] make-adder ;

: 40- ( -- value )
  [ dup 40 - ] make-adder ;

: 41+ ( -- value )
  [ dup 41 + ] make-adder ;

: 41- ( -- value )
  [ dup 41 - ] make-adder ;

: 42+ ( -- value )
  [ dup 42 + ] make-adder ;

: 42- ( -- value )
  [ dup 42 - ] make-adder ;

: 43+ ( -- value )
  [ dup 43 + ] make-adder ;

: 43- ( -- value )
  [ dup 43 - ] make-adder ;

: 44+ ( -- value )
  [ dup 44 + ] make-adder ;

: 44- ( -- value )
  [ dup 44 - ] make-adder ;

: 45+ ( -- value )