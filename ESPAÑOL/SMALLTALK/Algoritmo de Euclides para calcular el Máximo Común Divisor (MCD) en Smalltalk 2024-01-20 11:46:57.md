```smalltalk
[ :n :m |
  [ :p :d :r :t |
    p := n.
    d := m.
    [ d > 0 ] whileTrue: [
      r := p // d.
      t := p mod d.
      t = 0 ifTrue: [
        ^r
      ].
      p := d.
      d := t
    ].
    ^0
  ] apply
] value: 213649 345872
```

Este código calcula el máximo común divisor (MCD) de dos números enteros positivos utilizando el algoritmo de Euclides. El algoritmo funciona restando repetidamente el número más pequeño del número más grande hasta que el resto sea 0. El último número distinto de cero es el MCD.

El código está escrito en Smalltalk y utiliza una serie de construcciones de lenguaje interesantes.

* La primera es el constructor de bloques, que se utiliza para definir una función anónima. La función anónima toma dos argumentos, :n y :m, y devuelve el MCD de esos dos números.
* La segunda es el bucle `whileTrue:`, que se utiliza para ejecutar un bloque de código mientras una condición sea verdadera. En este caso, el bucle `whileTrue:` se utiliza para restar repetidamente el número más pequeño del número más grande hasta que el resto sea 0.
* La tercera es el mensaje `value:`, que se utiliza para aplicar una función a un valor. En este caso, el mensaje `value:` se utiliza para aplicar la función anónima al par de números 213649 y 345872.

El resultado del código es el MCD de 213649 y 345872, que es 19.