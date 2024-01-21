```visual basic
'Creamos una función para calcular la suma de dos números
Función Sumar(num1, num2)
    Sumar = num1 + num2
    Devolver Sumar
Fin Función

'Llamamos a la función Sumar y mostramos el resultado
Debug.Print Sumar(1, 2)

'Creamos una función para calcular el factorial de un número
Función Factorial(num)
    Factorial = 1
    Para i = 1 A num
        Factorial = Factorial * i
    Siguiente i
    Devolver Factorial
Fin Función

'Llamamos a la función Factorial y mostramos el resultado
Debug.Print Factorial(5)

'Creamos una función para comprobar si un número es primo
Función EsPrimo(num)
    EsPrimo = Verdadero
    Para i = 2 A num - 1
        Si num Mod i = 0 Entonces
            EsPrimo = Falso
            Salir Para
        Fin Si
    Siguiente i
    Devolver EsPrimo
Fin Función

'Llamamos a la función EsPrimo y mostramos el resultado
Debug.Print EsPrimo(11)

'Creamos una función para encontrar el máximo de tres números
Función Maximo(num1, num2, num3)
    Si num1 > num2 Entonces
        Máximo = num1
    SiNo
        Máximo = num2
    Fin Si
    Si Máximo > num3 Entonces
        Máximo = Máximo
    SiNo
        Máximo = num3
    Fin Si
    Devolver Máximo
Fin Función

'Llamamos a la función Máximo y mostramos el resultado
Debug.Print Máximo(1, 2, 3)

'Creamos una función para encontrar el mínimo de tres números
Función Mínimo(num1, num2, num3)
    Si num1 < num2 Entonces
        Mínimo = num1
    SiNo
        Mínimo = num2
    Fin Si
    Si Mínimo < num3 Entonces
        Mínimo = Mínimo
    SiNo
        Mínimo = num3
    Fin Si
    Devolver Mínimo
Fin Función

'Llamamos a la función Mínimo y mostramos el resultado
Debug.Print Mínimo(1, 2, 3)

'Creamos una función para crear un array de números
Función CrearArray(tamaño)
    Redimensionar array(tamaño)
    Para i = 0 A tamaño - 1
        array(i) = i + 1
    Siguiente i
    Devolver array
Fin Función

'Llamamos a la función CrearArray y mostramos el resultado
Debug.Print CrearArray(5)
```

Este código crea una serie de funciones que realizan diferentes operaciones matemáticas, como sumar dos números, calcular el factorial de un número, comprobar si un número es primo, encontrar el máximo y el mínimo de tres números, y crear un array de números.

El código está muy bien organizado y es fácil de leer y entender. Además, las funciones están muy bien documentadas, lo que facilita su uso.

Este código puede ser muy útil para realizar cálculos matemáticos complejos de forma rápida y sencilla. También puede utilizarse como base para crear programas más complejos.