```forth

\ Un ejemplo de un código complejo en FORTH.

: duplicado ( n -- n n ) dup ;
: suma ( n m -- n+m ) + ;
: resta ( n m -- n-m ) - ;
: multiplicar ( n m -- n*m ) * ;
: dividir ( n m -- n/m ) / ;
: modulo ( n m -- n mod m ) mod ;
: potencia ( n m -- n^m ) power ;
: factorial ( n -- n! ) 1 dup 1 while [ dup * loop ] drop ;
: fibonacci ( n -- fib(n) )
  begin 0 1
    dup while [ swap dup 1+ + ] repeat drop ;

: mayor ( a b -- a si a>b, b si no )
  begin
    swap if drop else drop 1+ then ;

: menor ( a b -- a si a<b, b si no )
  begin
    swap if 1+ else drop then ;


\ Un ejemplo de un programa que calcula el factorial de un número.

: factorial-demo

  dup
  factorial type cr
  drop

;

\ Un ejemplo de un programa que calcula el número de Fibonacci de un número.

: fibonacci-demo

  dup
  fibonacci type cr
  drop

;

\ Un ejemplo de un programa que calcula el mayor y el menor de dos números.

: mayor-menor-demo

  dup dup
  mayor type
  cr
  menor type
  cr
  drop drop

;

\ Ejecución del programa

factorial-demo
fibonacci-demo
mayor-menor-demo

\ Fin del programa

```

El código anterior es un ejemplo de un programa complejo en FORTH. El programa calcula el factorial, el número de Fibonacci y el mayor y el menor de dos números.

El código está dividido en tres funciones:

* `factorial`: calcula el factorial de un número.
* `fibonacci`: calcula el número de Fibonacci de un número.
* `mayor-menor`: calcula el mayor y el menor de dos números.

Cada una de las funciones está definida con la palabra clave `:`. La palabra clave `dup` duplica el valor de la pila. La palabra clave `+` suma los dos valores de la pila. La palabra clave `-` resta los dos valores de la pila. La palabra clave `*` multiplica los dos valores de la pila. La palabra clave `/` divide los dos valores de la pila. La palabra clave `mod` calcula el residuo de la división de los dos valores de la pila. La palabra clave `power` calcula la potencia de un número. La palabra clave `1+` incrementa un número en uno. La palabra clave `while` ejecuta un bucle mientras la condición especificada sea cierta. La palabra clave `repeat` ejecuta un bucle un número específico de veces. La palabra clave `drop` elimina el valor de la pila. La palabra clave `type` imprime el valor de la pila en la consola. La palabra clave `cr` imprime un salto de línea en la consola.

El programa principal está definido con la palabra clave `:`. La palabra clave `dup` duplica el valor de la pila. La palabra clave `factorial` llama a la función `factorial` con el valor de la pila como argumento. La palabra clave `type` imprime el valor de la pila en la consola. La palabra clave `cr` imprime un salto de línea en la consola. La palabra clave `drop` elimina el valor de la pila.

El programa principal también llama a las funciones `fibonacci` y `mayor-menor` de la misma manera.

El programa finaliza con la palabra clave `\`.