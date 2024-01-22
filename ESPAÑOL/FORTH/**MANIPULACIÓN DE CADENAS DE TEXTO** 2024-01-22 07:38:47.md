```forth

: _L' ( a b -- a' ) ;

: _R' ( a b -- b' ) ;

: _?` ( a b c -- c' )
  dup 0= if pop _L' else _R' then ;

: _!` ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then ;

: _@ ( a b c -- a' b' ) ;

: _T! ( a b c -- c' ) ;

: 2DROP ( a b -- ) 2drop ;

: 2SWAP ( a b c -- c b a ) 2swap ;

: 2OVER ( a b -- a b a b ) 2over ;

: U. ( a b -- )(Entrada) Ejecuta una secuencia de comandos que codifica la entrada. ;

: ^ ( a b -- c )(XOR) Realiza una operación XOR entre dos números. ;

: ~ ( a -- b )(NOT) Realiza una operación NOT sobre un número. ;

: U> ( a b -- ) (Salida) Ejecuta una secuencia de comandos que decodifica la entrada. ;

: $U ( a -- a' )
  U@ 2drop 2dup 0= if 0 exit then ; IMMEDIATE

: $! ( a b -- b' )
  U, 2drop ; IMMEDIATE

: _" ( a -- ) _> ;

: _^ ( a b -- c ) swap ^ ;

: _!`+ ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1+ 1 _!` 1- _!` ; IMMEDIATE

: _`+ ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1- 1 _!` 1+ _!` ; IMMEDIATE

: _`- ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1- 1 _!` -1 _!` ; IMMEDIATE

: _>! ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1+ 1 _!` 0 _!` ; IMMEDIATE

: _?< ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1- 1 _!` -1 _!` ; IMMEDIATE

: _!> ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1+ 1 _!` 1 _!` ; IMMEDIATE

: _?^ ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1+ 1 _!` 0 _!` ; IMMEDIATE

: _?< ( a b c -- c' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b 1- 1 _!` 1 _!` ; IMMEDIATE

: _& ( a b -- c ) b and ;

: _| ( a b -- c ) b or ;

: _? ( a b c -- c' )
  dup 0= if pop _L' else _R' then ;

: _@! ( a b c d -- c' d' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b c _T! 1+ _!` ; IMMEDIATE

: _!@ ( a b c d -- c' d' )
  dup 0= if pop _L' swap _R' else _R' swap _L' then exit 0>
  b c _@ 1+ _!` ; IMMEDIATE

: _"CR ( -- ) 13 _! 10 _! ;

: _"SPACE ( -- ) 32 _! ;

: _"WORD ( a -- ) 2dup 0= if drop exit then
  s" " 0 _! 0 _! swap do
    over swap s" " _!
    1+ dup 0= until
  drop ; IMMEDIATE

: _"DEC ( a -- )
  swap 0 > if swap _" sign then
  0 swap
  dup _" radix 10 % 0<= if drop 0 else _" digit then do
    dup 10 * swap over - swap _" digit
    1+ dup 0= until
  drop ;

: _"HEX ( a -- )
  swap dup 0> if 0 _" sign else 0 _! then
  0 swap
  dup _" radix 16 % 0<= if drop 0 else _" digit then do
    dup 16 * swap over - swap _" digit
    1+ dup 0= until
  drop ;

: _S->U ( a -- )
  /string U. 1000 + ;

: _U->S ( a -- )
  1000 - U> .string ;

: _DECIMAL->STRING ( a -- )
  0 _" radix swap _"DEC _S->U ;

: _HEXADECIMAL->STRING ( a -- )
  16 _" radix swap _"HEX _S->U ;

: _STRING->DECIMAL ( a -- )(Cadena a Decimal) Convierte una cadena de dígitos en un número decimal. ;
  0 dup _S->U 10 _" radix ! \
  10 swap do
    10 * swap _S->U swap -
    0> if 1+ exit then
  loop
  drop ;

: _STRING->HEXADECIMAL ( a -- )(Cadena a Hexadecimal) Convierte una cadena de dígitos en un número hexadecimal. ;
  0 dup _S->U 16 _" radix ! \
  16 swap do
    16 * swap _S->U swap -
    0> if 1+ exit then
  loop
  drop ;

: _STRING>D ( a -- d )(Cadena a Decimal) Convierte una cadena de dígitos en un número decimal. ;
  0 dup _S->U 10 _" radix ! \
  10 swap do
    10 * swap _S->U swap -
    0> if 1+ exit then
  loop
  drop ;

: _STRING>H ( a -- h )(Cadena a Hexadecimal) Convierte una cadena de dígitos en un número hexadecimal. ;
  0 dup _S->U 16 _" radix ! \
  16 swap do
    16 * swap _S->U swap -
    0> if 1+ exit then
  loop
  drop ;

: _STRING>I ( a -- i )(Cadena a Entero) Convierte una cadena de dígitos en un número entero. ;
  0 dup _S->U 10 _" radix ! \
  10 swap do
    10 * swap _S->U swap -
    0> if 1+ exit then
  loop
  drop ;

: _STRING>U. ( a -- u. )(Cadena a Entrada) Convierte una cadena de dígitos en una entrada. ;
  0 dup _S->U \
  1000 + _! ` U. 0 _! ;

: _U.>STRING ( u. -- )(Entrada a Cadena) Convierte una entrada en una cadena de dígitos. ;
  0> if 0 _" sign else drop then
  dup _" radix dup
  10 * 10 swap do
    i 0>= if _" digit else _" sign then loop drop
  dup _" radix swap
  16 * 16 swap do
    i 0>= if _" digit else _" sign then loop drop
  drop ;

: _U.>STRING ( u. -- )(Entrada a Cadena) Convierte una entrada en una cadena de dígitos. ;
  0> if 0 _" sign else drop then
  dup _" radix dup
  10 * 10 swap do
    i 0>= if _" digit else _" sign then loop drop
  dup _" radix swap
  16 * 16 swap do
    i 0>= if _" digit else _" sign then loop drop
  drop ;

: _"TYPE ( a -- )
  0 swap dup 0= if 0 _! then
  dup _" radix 10 % 0<= if drop 0 else _" digit then do
    over swap s" " _! dup 0= if 0 _! exit then
    1+ dup 0= until
  drop ; IMMEDIATE

: _"TYPE ( a -- )
  0 swap dup 0= if 0 _! then
  dup _" radix 16 % 0<= if drop 0 else _" digit then do
    over swap s" " _! dup 0= if 0 _! exit then
    1+ dup 0= until
  drop ; IMMEDIATE

: _D. ( a -- )
  dup _" radix 10 _"DECIMAL swap _"TYPE ;

: _H. ( a -- )
  dup _" radix 16 _"HEXADECIMAL swap _"TYPE ;

: _U. ( a -- )
  _"TYPE ;

: _"STRING ( s -- )
  s" " 0 swap _! s" " 0 _! do
    i @ s@ _! 1+ loop ; IMMEDIATE

: _"STRING ( s -- )
  s" " 0 _! s" " 0 _! do
    i @ s@ _! 1+ loop ; IMMEDIATE

: _"SUBSTRING ( s a b -- )(Subcadena) Extrae una subcadena de una cadena. ;
  s" " dup +
  b - 1 _"STRING ;

: _STRING= ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ = swap !
    1+ loop
  0> ;

: _STRING<> ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ = swap !
    1+ loop
  0> ;

: _STRING< ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ < swap !
    1+ loop
  0> ;

: _STRING<= ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ <= swap !
    1+ loop
  0> ;

: _STRING> ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ > swap !
    1+ loop
  0> ;

: _STRING>= ( s1 s2 -- f )
  dup @ ?do
    s1 @ s2 @ >= swap !
    1+ loop
  0> ;

: _STRING/SEARCH ( s c -- i )
  s" " 0 _! s" " 0 _! over
  do
    dup i @
    c @ = swap ! 1+
    i @ + swap 0= until
  drop ;

: _STRING/INSERT ( s p c -- )(Insertar) Inserta un caracter en una cadena. ;
  dup @ ?do
    dup s@ over swap ! i + !
    s@ c @ = swap !
    i @ + 1+ loop drop ;

: _STRING/DELETE ( s p -- )(Borrar) Borra un caracter en una cadena. ;
  dup @ ?do
    dup i @ + s@ swap !
    i @ + 1+ loop drop ;

: _STRING/REPLACE ( s p c -- )(Reemplazar) Reemplaza un caracter en una cadena. ;
  dup @ ?do
    s@ c @ = swap !
    i + !
    i @ + 1+ loop drop ;

: _STRING/TRIM ( s -- )(Recortar) Recorta los espacios en blanco al principio y al final de una cadena. ;
  0 do
    s@ 32 = if 1+ then
  loop drop
  over negate
  do
    dup i @
    s@ 32 = swap !
    1- loop drop ;

: _STRING/UPPER ( s -- )(Mayúsculas) Convierte todos los caracteres de una cadena a mayúsculas. ;
  @ ?do
    s@ 97 > 122 < if s@ 32 - then
    1+ loop ;

: _STRING/LOWER ( s -- )(Minúsculas) Convierte todos los caracteres de una cadena a minúsculas. ;
  @ ?do
    s@ 65 > 90 < if s@ 32 + then
    1+ loop ;


: _STRING/CAPITALIZE ( s -- )(Capitalizar) Capitaliza la primera letra de una cadena y convierte el resto a minúsculas. ;
  s@ 97 > 122 < if s@ 32 - then
  do
    s@ 97 > 122 < if s@ 32 + then
    1+ loop ;
```