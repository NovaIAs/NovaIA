```forth
: decir ( -- s )
  dup
  type cr
;

: sumar ( a b -- a+b )
  +
;

: restar ( a b -- a-b )
  -
;

: multiplicar ( a b -- a*b )
  *
;

: dividir ( a b -- a/b )
  /
;

: modulo ( a b -- a%b )
  mod
;

: y ( a b -- a&b )
  and
;

: o ( a b -- a|b )
  or
;

: no ( a -- !a )
  not
;

: menor ( a b -- a<b )
  <
;

: menor-o-igual ( a b -- a<=b )
  <=
;

: mayor ( a b -- a>b )
  >
;

: mayor-o-igual ( a b -- a>=b )
  >=
;

: igual ( a b -- a==b )
  =
;

: diferente ( a b -- a!=b )
  <>
;

: saltar ( n -- )
  emit
;

: duplicar ( a -- a a )
  dup
;

: intercambiar ( a b -- b a )
  swap
;

: rotar ( a b c -- b c a )
  rot
;

: vuelta-atras ( a b c -- c b a )
  rrot
;

: drop ( a -- )
  drop
;

: duplicar-y-soltar ( a -- a )
  dup drop
;

: intercambiar-y-soltar ( a b -- )
  swap drop
;

: rotar-y-soltar ( a b c -- b c )
  rot drop
;

: vuelta-atras-y-soltar ( a b c -- c b )
  rrot drop
;

: crear-espacio ( n -- addr )
  alloc
;

: liberar-espacio ( addr -- )
  free
;

: almacenar ( addr a -- )
  @ store !
;

: recuperar ( addr -- a )
  @
;

: celda ( n addr -- a )
  + @
;

: celda! ( n addr a -- )
  + !
;

: bloque ( n addr -- n addr )
  ,
;

: bloque! ( n addr n a -- )
  , !
;

: ejecutar-primitiva ( n -- )
  execute
;

: compilar-palabra ( nombre cuerpo -- )
  { nombre } body create
  does>
;

: interpretar-palabra ( nombre -- )
  execute immediate
;

: definir-palabra ( nombre cuerpo -- )
  compilar-palabra interpretar-palabra
;

: si-entonces ( condición cuerpo -- )
  if >then
;

: si-no ( condición cuerpo-verdadero cuerpo-falso -- )
  if >then else
;

: mientras-hacer ( condición cuerpo -- )
  begin
    while
      repeat
    end
  ;

: para ( n inicio fin -- )
  begin
        do
          loop
    repeat
  end
;

: guardar-estado ( p -- )
  save-state
;

: restaurar-estado ( -- p )
  restore-state
;

: abortar ( -- )
  abort
;

: salir ( -- )
  exit
;
```