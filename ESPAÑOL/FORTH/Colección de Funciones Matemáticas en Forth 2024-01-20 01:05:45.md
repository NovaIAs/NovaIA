```forth

\ Define una constante llamada 'PI' con el valor de π.
PI constant 3.14159265

\ Define una función llamada 'area_circulo' que calcula el área de un círculo.
: area_circulo ( radio -- area )
  PI * radio ^ 2 ;

\ Define una función llamada 'perimetro_circulo' que calcula el perímetro de un círculo.
: perimetro_circulo ( radio -- perimetro )
  2 * PI * radio ;

\ Define una función llamada 'volumen_cilindro' que calcula el volumen de un cilindro.
: volumen_cilindro ( radio altura -- volumen )
  area_circulo * altura ;

\ Define una función llamada 'superficie_cilindro' que calcula la superficie de un cilindro.
: superficie_cilindro ( radio altura -- superficie )
  perimetro_circulo * altura + 2 * area_circulo ;

\ Define una función llamada 'es_primo' que comprueba si un número es primo.
: es_primo ( n -- boolean )
  1 >if 0 exitthen ; 2 >if true exitthen ;
  dup 2 swap /mod 0 = if true exitthen ;
  3 2 swap /mod 0 = if true exitthen ;
  \ Comprueba los números impares hasta la raíz cuadrada de 'n'.
  2 * swap sqrt round + 1 abegin
    2 swap /mod 0 = if false exitthen ;
  arepeat ;
  true ;

\ Define una función llamada 'factorizar' que factoriza un número.
: factorizar ( n -- lista_de_factores )
  2 swap /mod 0 = if [ 2 drop ] while
    [ 3 >= while [ swap 3 /mod 0 = ] repeat
      [ 2 pick * swap pick / ] repeat
    ] repeat ;

\ Define una función llamada 'mcd' que calcula el máximo común divisor de dos números.
: mcd ( m n -- mcd )
  0 >if drop 0 exitthen ;
  dup 0 = if exitthen ;
  over mod recurse swap mod ;

\ Define una función llamada 'mcm' que calcula el mínimo común múltiplo de dos números.
: mcm ( m n -- mcm )
  over mcd * ;

\ Define una función llamada 'suma_digitos' que calcula la suma de los dígitos de un número.
: suma_digitos ( n -- suma )
  [ 0 ] while
    dup 10 mod swap +
    10 /
  repeat ;

\ Define una función llamada 'es_capicua' que comprueba si un número es capicúa.
: es_capicua ( n -- boolean )
  dup reverse = ;

\ Define una función llamada 'cadena_a_entero' que convierte una cadena de dígitos en un entero.
: cadena_a_entero ( cadena -- entero )
  0 [ '0' swap char- >number swap + ] for ;

\ Define una función llamada 'entero_a_cadena' que convierte un entero en una cadena de dígitos.
: entero_a_cadena ( entero -- cadena )
  dup 0 = if [ '0' cr ] exitthen ;
  [ 10 /mod char + ] while
  reverse ;

\ Define una función llamada 'ordenar' que ordena una lista de números.
: ordenar ( lista -- lista_ordenada )
  [ ] begin
    over swap dup > if [ dup swap ] then
    [ over swap < if [ drop swap ] then ] repeat
  while
  drop ;

\ Define una función llamada 'buscar' que busca un elemento en una lista.
: buscar ( lista elemento -- posición )
  [ 0 ] while
    dup = if [ exitthen ] then
    swap 1 +
  repeat
  0 ;

\ Define una función llamada 'eliminar' que elimina un elemento de una lista.
: eliminar ( lista elemento -- lista_sin_elemento )
  [ ] begin
    over swap = if [ drop swap ] then
  while
  drop ;

\ Define una función llamada 'insertar' que inserta un elemento en una lista.
: insertar ( lista elemento -- lista_con_elemento )
  [ ] begin
    over swap > if [ dup swap ] then
    [ over swap < if [ drop swap ] then ] repeat
  while
  swap ;

```