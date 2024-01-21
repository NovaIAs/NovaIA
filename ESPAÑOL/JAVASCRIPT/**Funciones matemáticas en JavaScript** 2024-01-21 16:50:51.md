```javascript
// Función que devuelve un número aleatorio entre un rango dado
function numeroAleatorio(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Función que devuelve los números primos hasta un número dado
function numerosPrimos(numero) {
    const numerosPrimos = [];
    for (let i = 2; i <= numero; i++) {
        let esPrimo = true;
        for (let j = 2; j <= Math.sqrt(i); j++) {
            if (i % j === 0) {
                esPrimo = false;
                break;
            }
        }
        if (esPrimo) {
            numerosPrimos.push(i);
        }
    }
    return numerosPrimos;
}

// Función que devuelve los números perfectos hasta un número dado
function numerosPerfectos(numero) {
    const numerosPerfectos = [];
    for (let i = 1; i <= numero; i++) {
        let sumaDivisores = 0;
        for (let j = 1; j < i; j++) {
            if (i % j === 0) {
                sumaDivisores += j;
            }
        }
        if (sumaDivisores === i) {
            numerosPerfectos.push(i);
        }
    }
    return numerosPerfectos;
}

// Función que devuelve los números de Fibonacci hasta un número dado
function fibonacci(numero) {
    const fibonacci = [0, 1];
    while (fibonacci[fibonacci.length - 1] < numero) {
        const siguienteNumero = fibonacci[fibonacci.length - 1] + fibonacci[fibonacci.length - 2];
        fibonacci.push(siguienteNumero);
    }
    return fibonacci;
}

// Función que devuelve el factorial de un número dado
function factorial(numero) {
    let factorial = 1;
    for (let i = 1; i <= numero; i++) {
        factorial *= i;
    }
    return factorial;
}

// Función que devuelve la suma de los dígitos de un número dado
function sumaDigitos(numero) {
    let sumaDigitos = 0;
    while (numero > 0) {
        sumaDigitos += numero % 10;
        numero = Math.floor(numero / 10);
    }
    return sumaDigitos;
}

// Función que devuelve el número de dígitos de un número dado
function numeroDigitos(numero) {
    let numeroDigitos = 0;
    while (numero > 0) {
        numeroDigitos++;
        numero = Math.floor(numero / 10);
    }
    return numeroDigitos;
}

// Función que devuelve el número inverso de un número dado
function numeroInverso(numero) {
    let numeroInverso = 0;
    while (numero > 0) {
        numeroInverso = numeroInverso * 10 + numero % 10;
        numero = Math.floor(numero / 10);
    }
    return numeroInverso;
}
```

Este código contiene una serie de funciones matemáticas que devuelven diferentes resultados en función de los parámetros que se les pasan.

La primera función, `numeroAleatorio`, devuelve un número aleatorio entre un rango dado.

La segunda función, `numerosPrimos`, devuelve los números primos hasta un número dado.

La tercera función, `numerosPerfectos`, devuelve los números perfectos hasta un número dado.

La cuarta función, `fibonacci`, devuelve los números de Fibonacci hasta un número dado.

La quinta función, `factorial`, devuelve el factorial de un número dado.

La sexta función, `sumaDigitos`, devuelve la suma de los dígitos de un número dado.

La séptima función, `numeroDigitos`, devuelve el número de dígitos de un número dado.

La octava función, `numeroInverso`, devuelve el número inverso de un número dado.