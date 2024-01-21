```scheme
**(define (factorial n)
    **(if (= n 1)
        **1
        **(factorial (sub1 n) *) n))

**(define (fibonacci n)
    **(if (< n 2)
        **n
        **(fibonacci (sub1 n) *) (fibonacci (sub1 (sub1 n)))))

**(define (suma-divisores n)
    **(cond
        **((= n 0) 0)
        **((= n 1) 1)
        **(else (suma-divisores (sub1 n))) 1))

**(define (primo? n)
    **(cond
        **((= n 2) #t)
        **((< n 2) #f)
        **(else (and (zero? (modulo n 2)) (not (divisor? 3 n))) (primo? (sub1 n))))

**(define (divisor? d n)
    **(cond
        **((= n 0) #f)
        **((= n 1) #f)
        **(else (zero? (modulo n d))) (zero? (modulo n d))))

**(define (mayor-divisor n)
    **(cond
        **((= n 0) 0)
        **((= n 1) 1)
        **(else (mayor-divisor (sub1 n))) (sub1 n))
```

Este código en SCHEME incluye varias funciones matemáticas complejas:

- `(factorial n)`: Calcula el factorial de un número `n` mediante recursión.
- `(fibonacci n)`: Calcula el `n`-esimo número de Fibonacci utilizando también recursión.
- `(suma-divisores n)`: Calcula la suma de todos los divisores positivos de un número `n`.
- `(primo? n)`: Determina si un número `n` es primo o no.
- `(divisor? d n)`: Comprueba si un número `d` es divisor de un número `n`.
- `(mayor-divisor n)`: Encuentra el mayor divisor común de un número `n`.