```forth
: PALABRA ( -- )
  dup @
  begin
    over swap dup
    swap @
    nip cells @
    cells @
    tuck swap + tuck [CHAR] drop
    dup literal>
    WHILE drop
  REPEAT
  drop ;

: ASCII-MAYUSCULAS ( -- )
  dup
  begin
    ascii>= @
    ascii<= @
    swap
    swap
    ascii>= @
    swap
    ascii<= @
    WHILE [CHAR] + swap drop
  REPEAT
  drop ;

: VARIABLES ( -- ")
  "  Pi(π)    =   3.14159265"
  "  Gravedad  = -9.80665 m/s^2"
  "  Velocidad  = 9.8 m/s"
  "  C         =   3.0x10^8 m/s"
  "  Fahrenheit = 32 grados F"
  "  Kelvin     =   273.15 K" ;

: MENSAJE ( -- "No hay mensaje definido" )
  "No hay mensaje definido" ;

: MENSAJE1 ( -- "Hola, mundo!" )
  "Hola, mundo!" ;

: MENSAJE2 ( -- "Aquí hay otra cosa!" )
  "Aquí hay otra cosa!" ;

: SELECCIONA-MENSAJE ( -- )
  0 [CHAR] 4 - IF
    MENSAJE1
  ELSE
    MENSAJE2
  THEN ;

: MENSAJES ( -- )
  " Selecciona un mensaje:"
  CR
  " (0) Hola, mundo!"
  CR
  " (1) Aquí hay otra cosa!"
  CR
  SELECCIONA-MENSAJE ;

: CUADRADO-MAGICO ( -- )
  6 7 2   1 5 9   8 3 4
  3 8 9   5 6 7   4 2 1
  2 9 4   7 3 8   6 5 1 ;

: MATRIZ-IMPRIMIR ( n -- )
  0
  do
    i constant
    swap
    i @ [CHAR] + 2dup swap
    tuck
    emit
    dup
    n = if CR then
  loop
  drop ;

: CUADRADO-MAGICO-IMPRIMIR ( -- )
  15 CUADRADO-MAGICO
  MATRIZ-IMPRIMIR ;

: EJECUTA-MENSAJE ( fin -- )
  [CHAR] swap
  cell>
  begin
    over swap >= if
      drop exit then
    over swap = if
      fin then
    [CHAR] swap
    execute then
  repeat
  drop
  fin ;

: ENCUENTRA-ULTIMO ( a -- n )
  0
  begin
    tuck dup @
    swap @ = UNTIL
    drop
  repeat
  1- ;

: ELIMINA-DUPLICADOS ( a -- )
  nip swap
  cell>
  begin
    over swap >= if
      drop exit then
    dup @
    swap
    find
    over swap >= if
      tuck ENCUENTRA-ULTIMO tuck
      2drop
      swap over
      >r @ swap <r !
      r> swap
      nip
      tuck
    then
    swap
  repeat
  drop ;

: FIZZ-BUZZ ( -- )
  1 100 do
    i
    2 0 mod 3 0 mod and
    if
      "FizzBuzz" emit
    else
      2 0 mod
      if
        "Fizz" emit
      else
        3 0 mod
        if
          "Buzz" emit
        else
          dup
          emit
        then
      then
    then
    CR
  loop
  drop ;

: MAYORES-QUE ( x -- lista )
  cell>
  begin
    over swap >= if
      tuck
      over swap = if
        drop exit then
      over swap >
      if
        swap drop
      then
    else
      drop exit
    then
  repeat
  drop ;

: SALUDO-PERSONALIZADO ( nombre -- )
  2dup "Hola" emit
  2drop CR
  1+ "te doy la bienvenida al club" emit
  1+ CR ;

: SALUDO ( -- )
  "Escribe tu nombre:"
  CR
  dup 80 KEY
  14 [CHAR] =
  if
    "No has escrito nada."
    CR
    drop exit
  else
    swap
    14 [CHAR] type
    drop
  then
  dup "!"
  tuck swap
  SALUDO-PERSONALIZADO ;

: LANZAR-DADO ( -- n )
  1 6 random + ;

: LANZAR-DADOS ( -- n )
  0
  begin
    dup @
    LANZAR-DADO +
    tuck
    dup
    100 = if
      EXIT
    else
      tuck
    then
  repeat
  drop
  tuck ;

: RESULTADOS-DADOS ( -- )
  0 99 do
    i
    cell>
    begin
      tuck dup @
      swap @ =
      UNTIL
      drop
    repeat
    1+
    [CHAR] + emit
    tuck
    20 mod
    if
      CR
    then
  loop
  drop
  CR ;

: LISTA-PRIMOS ( -- )
  2 10000 do
    i
    2dup @
    dup
    2 1 swap % 0 = not and 2 1 swap % 0 = not or
    if
      [CHAR] + emit
    then
  loop
  drop
  CR ;

: PADRES ( -- )
  " Juan"
  "María"
  "Sara"
  "Luis" ;

: HIJOS ( -- )
  " Ana"
  " José"
  " Pedro"
  " María" ;

: FAMILIA ( -- )
  PADRES
  dup
  dup
  ELIMINA-DUPLICADOS
  swap
  dup
  HIJOS
  ELIMINA-DUPLICADOS
  dup
  ELIMINA-DUPLICADOS
  swap
  swap
  dup
  tuck
  2dup
  MAYORES-QUE
  swap
  drop
  swap
  tuck
  2dup
  MAYORES-QUE
  swap
  drop
  3dup
  PADRES
  ELIMINA-DUPLICADOS
  swap
  drop
  3dup
  HIJOS
  ELIMINA-DUPLICADOS
  swap
  drop
  swap
  dup
  "Familia:" emit
  CR
  dup
  "  Padres:" emit
  CR
  swap
  "    "
  tuck swap
  PALABRA
  CR
  dup
  "  Hijos:" emit
  CR
  swap
  "    "
  tuck swap
  PALABRA
  CR
  dup
  "  Miembros:" emit
  CR
  swap
  "    "
  tuck swap
  PALABRA
  CR
  nip
  nip
  nip
  drop ;

: PORCENTAJE ( n -- % )
  100 div [CHAR] + emit ;

: TIEMPO ( -- )
  @ 110
  300
  mod
  dup
  10 > if
    10 swap -
    "0"
  then
  tuck
  54 %
  dup
  10 > if
    10 swap -
    "0"
  then
  "  :hh:mm:ss" emit
  tuck
  54 %
  tuck
  24 %
  3depth swap
  2dup
  tuck
  swap
  3dup
  swap
  tuck
  tuck
  3depth swap
  3dup
  swap
  2dup
  tuck
  swap
  tuck
  3depth
  swap
  tuck
  swap
  tuck
  swap
  swap
  tuck
  swap
  2drop
  3drop
  tuck
  tuck
  3depth swap
  3depth swap
  cells @ cells @
  cells @ cells @ +
  swap
  drop
  3drop
  2drop
  2drop
  CR ;

: HORAS-MINUTOS-SEGUNDOS ( -- n )
  60
  mod
  swap
  60
  mod ;

: EJERCICIO ( -- )
  "Hola, mundo!" emit CR
  MENSAJES
  SELECCIONA-MENSAJE
  EJECUTA-MENSAJE
  CR
  "Pi(π) = "
  3.14159265 emit CR
  "Gravedad = "
  -9.80665 emit
  " m/s^2" emit CR
  "Velocidad = "
  9.8 emit
  " m/s" emit CR
  "C = "
  3 10 ^ 8 emit
  " m/s" emit CR
  "Fahrenheit = "
  32 emit
  " grados F" emit CR
  "Kelvin = "
  273.15 emit
  " K" emit CR
  CR
  "Cuadrado mágico:"
  CR
  CUADRADO-MAGICO-IMPRIMIR
  CR
  FIZZ-BUZZ
  CR
  "Lista de primos:"
  CR
  LISTA-PRIMOS
  CR
  FAMILIA
  CR
  TIEMPO
  CR ;
```