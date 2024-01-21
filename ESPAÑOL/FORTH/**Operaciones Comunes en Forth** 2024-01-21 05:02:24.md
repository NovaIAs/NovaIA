```forth

\ INVERTIR UNA CADENA DE TEXTO
: invertir ( cadena -- cadena-invertida )
  dup longitud
  0 rot
  [
    dup
    swap
    dup @
    swap rot dup @
    swap rot @
    swap rot @
    swap rot @
  ]
  rot swap rot drop drop ;

\ OBTENER LA FECHA Y HORA ACTUAL
: fecha-y-hora ( -- fecha hora )
  fecha .
  hora .
  cr ;

\ DETERMINAR SI UN NÚMERO ES PAR O IMPAR
: es-par ( número -- true|false )
  dup 2 mod 0 = ;

\ CALCULAR EL MÁXIMO COMÚN DIVISOR DE DOS NÚMEROS
: mcd ( número número -- número )
  [
    dup rot swap mod
    swap dup 0 >
    while [
      swap drop
    ]
    repeat
  ] ;

\ GENERAR UNA LISTA DE NÚMEROS PRIMOS HASTA UN LÍMITE
: primos-hasta ( límite -- lista-de-primos )
  2 primos-hasta [
    swap dup 0 =
    while [
      drop
    ]
    repeat
    3 2 + dup
    dup límite >
    while [
      rot hasta-límite
      2dup * 2drop
      swap
    ]
    repeat ;

\ OBTENER EL NÚMERO DE VOCABULARIOS DEFINIDOS
: vocabularios-definidos ( -- número )
  vocabularios >r count r> ;

\ LISTAR TODOS LOS VOCABULARIOS DEFINIDOS
: vocabularios-listar ( -- )
  vocabularios >r
  dup 0 =
  while [
    @ . cr
  ]
  repeat r> ;

```