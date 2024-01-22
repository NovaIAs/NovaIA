```forth
: Sumar ( n1 n2 -- suma )
   + ;

: Restar ( n1 n2 -- diferencia )
   - ;

: Multiplicar ( n1 n2 -- producto )
   * ;

: Dividir ( n1 n2 -- cociente )
   / ;

: Igual ( n1 n2 -- bandera )
   = ;

: Menor ( n1 n2 -- bandera )
   < ;

: Mayor ( n1 n2 -- bandera )
   > ;

: MenorOIgual ( n1 n2 -- bandera )
   <= ;

: MayorOIgual ( n1 n2 -- bandera )
   >= ;

: And ( b1 b2 -- bandera )
   and ;

: Or ( b1 b2 -- bandera )
   or ;

: Negación ( b -- bandera )
   not ;

: SiEntonces ( condición acción )
   if ;

: SiEntoncesSiNo ( condición acción1 acción2 )
   ifelse ;

: Mientras ( condición acción )
   while ;

: HastaQue ( condición acción )
   until ;

: Para ( variable limite acción )
   do ;

: ParaInv ( variable limite acción )
   loop ;

: CreaArray ( tamaño dirección-array -- )
   allot ;

: AsignarArray ( índice valor dirección-array -- )
   @ ;

: ObtenerArray ( índice dirección-array -- valor )
   ! ;

: ImprimirArray ( tamaño dirección-array -- )
   do
      @ .
      loop ;

: Swap ( n1 n2 -- n2 n1 )
   swap ;

: Dup ( n -- n n )
   dup ;

: Inversa ( palabra -- )
   reverse ;

: Buscar ( elemento dirección-array tamaño -- índice )
   find ;

: Ordenar ( dirección-array tamaño -- )
   sort ;

: Salir ( -- )
   bye ;
```

Este código Forth implementa una serie de funciones y estructuras de control comunes en muchos lenguajes de programación.

Las funciones matemáticas incluyen sumar, restar, multiplicar y dividir.

Las estructuras de control incluyen if, elseif, while, until, do y loop.

También hay funciones para crear y gestionar arrays, así como para buscar y ordenar elementos en un array.

Por último, el código incluye una función para salir del intérprete Forth.

Este código es un ejemplo de cómo se pueden utilizar las características únicas de Forth para crear código compacto y eficiente.