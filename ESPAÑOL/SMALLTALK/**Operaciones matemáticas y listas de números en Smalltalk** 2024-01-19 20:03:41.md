```smalltalk
Sistema mostrarMensaje: 'Hola, mundo!'

Sistema mostrarMensaje: 'El resultado de 3 + 4 es',
    (3 + 4) imprimir

Sistema mostrarMensaje: 'El factorial de 5 es',
    (5 factorial) imprimir

Sistema mostrarMensaje: 'Los primeros 10 números de Fibonacci son',
    (1 a: 10) collect: [:i | (i fib)] imprimir

Sistema mostrarMensaje: 'El máximo común divisor de 12 y 18 es',
    (12 mcd 18) imprimir

Sistema mostrarMensaje: 'El mínimo común múltiplo de 9 y 15 es',
    (9 mcm 15) imprimir

Sistema mostrarMensaje: 'La raíz cuadrada de 16 es',
    (16 sqrt) imprimir

Sistema mostrarMensaje: 'El valor absoluto de -5 es',
    (-5 abs) imprimir

Sistema mostrarMensaje: 'El seno de 30 grados es',
    (30 degrees sin) imprimir

Sistema mostrarMensaje: 'El coseno de 45 grados es',
    (45 degrees cos) imprimir

Sistema mostrarMensaje: 'La tangente de 60 grados es',
    (60 degrees tan) imprimir

Sistema mostrarMensaje: 'El arco seno de 0.5 es',
    (0.5 asin) imprimir

Sistema mostrarMensaje: 'El arco coseno de 0.7071 es',
    (0.7071 acos) imprimir

Sistema mostrarMensaje: 'El arco tangente de 1 es',
    (1 atan) imprimir

Sistema mostrarMensaje: 'El logaritmo base 10 de 100 es',
    (100 log) imprimir

Sistema mostrarMensaje: 'El logaritmo natural de e es',
    (e ln) imprimir

Sistema mostrarMensaje: 'El número aleatorio entre 1 y 10 es',
    (1 to: 10) random imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números primos es',
    (1 to: 5) collect: [:i | (i prime)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números perfectos es',
    (1 to: 5) collect: [:i | (i perfect)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Mersenne primos es',
    (1 to: 5) collect: [:i | (i mersennePrime)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Fibonacci primos es',
    (1 to: 5) collect: [:i | (i fibonacciPrime)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Catalan es',
    (1 to: 5) collect: [:i | (i catalan)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Bell es',
    (1 to: 5) collect: [:i | (i bell)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Stirling del primer tipo es',
    (1 to: 5) collect: [:i | (i stirling1)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Stirling del segundo tipo es',
    (1 to: 5) collect: [:i | (i stirling2)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Bernoulli es',
    (1 to: 5) collect: [:i | (i bernoulli)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Euler es',
    (1 to: 5) collect: [:i | (i euler)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Genocchi es',
    (1 to: 5) collect: [:i | (i genocchi)] imprimir

Sistema mostrarMensaje: 'La lista de los primeros 5 números de Bell primos es',
    (1 to: 5) collect: [:i | (i bellPrime)] imprimir
```

Explicación del código:

* El código comienza con la línea `Sistema mostrarMensaje: 'Hola, mundo!'`, que muestra el mensaje "Hola, mundo!" en la consola.
* A continuación, el código muestra una serie de mensajes en la consola, cada uno de los cuales va acompañado de un cálculo matemático.
* Los cálculos matemáticos utilizan la sintaxis de Smalltalk para realizar operaciones aritméticas, trigonométricas y otras operaciones matemáticas.
* El código también utiliza la biblioteca estándar de Smalltalk para realizar cálculos matemáticos más avanzados, como el cálculo del factorial de un número, el máximo común divisor de dos números, el mínimo común múltiplo de dos números, la raíz cuadrada de un número, el valor absoluto de un número, el seno de un ángulo, el coseno de un ángulo, la tangente de un ángulo, el arco seno de un número, el arco coseno de un número, el arco tangente de un número, el logaritmo base 10 de un número, el logaritmo natural de un número, el número aleatorio entre dos números, la lista de los primeros números primos, la lista de los primeros números perfectos, la lista de los primeros números de Mersenne primos, la lista de los primeros números de Fibonacci primos, la lista de los primeros números de Catalan, la lista de los primeros números de Bell, la lista de los primeros números de Stirling del primer tipo, la lista de los primeros números de Stirling del segundo tipo, la lista de los primeros números de Bernoulli, la lista de los primeros números de Euler, la lista de los primeros números de Genocchi y la lista de los primeros números de Bell primos.
* El código finaliza con la línea `Sistema mostrarMensaje: 'La lista de los primeros 5 números de Bell primos es', (1 to: 5) collect: [:i | (i bellPrime)] imprimir`, que muestra la lista de los primeros 5 números de Bell primos en la consola.